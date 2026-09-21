;;; ==========================================================================
;;; コマンド名 : STRETCH_DIM_BOTH （短縮エイリアス: SDB）
;;; 概要       : 選択した長さ寸法（DIMENSION）の両端を、指定した数値ずつ
;;;              外側へ広げ（両側合計で2倍延長）、寸法線および接続線分を連動ストレッチする
;;; 動作環境   : AutoCAD / BricsCAD 等の AutoLISP 環境
;;; 仕様       :
;;;   1. 長さ寸法（回転寸法[水平・垂直・斜め]・平行寸法）を複数選択可能
;;;   2. 片側の拡張量を指定可能（デフォルト: 50、前回値を記憶）
;;;      例: 50 を指定した場合、両端がそれぞれ 50 ずつ広がり合計 +100 延長
;;;   3. DXF定義点(13/14)を直接操作することで、ActiveXのプロパティ未サポートエラーを完全解消
;;;   4. 寸法の両端に接続している線分（LINE）の端点も自動検出して連動ストレッチ
;;;   5. テキスト上書き（DXF 1）がある場合はクリアし最新の実測値を表示
;;;   6. 1回の Undo で全変更を元に戻すことが可能（Undoマークの安全管理）
;;;   7. 中断時（Escキー等）のエラー処理（*error*）を完備
;;; ==========================================================================

(vl-load-com)

;; 前回の片側拡張量を記憶するグローバル変数（未設定時は 50.0）
(if (null *STRETCH_DIM_INC*)
  (setq *STRETCH_DIM_INC* 50.0)
)

(defun c:STRETCH_DIM_BOTH ( / *error* acadObj doc oldCmd ss i ent elist dimType 
                               p1 p2 newP1 newP2 v1 v2 dx dy dz dist 
                               ux uy uz rot cosA sinA dot dir vOut 
                               count lineCount userInc incVal dimDec 
                               curText formatVal stretchConnectedLines )

  ;; ------------------------------------------------------------------------
  ;; エラーハンドラ
  ;; ------------------------------------------------------------------------
  (defun *error* (msg)
    (if (and doc (= (type doc) 'VLA-OBJECT))
      (vla-EndUndoMark doc)
    )
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (if (and msg (not (wcmatch (strcase msg t) "*cancel*,*exit*,*quit*")))
      (princ (strcat "\n[STRETCH_DIM_BOTH] エラー: " msg))
    )
    (princ)
  )

  ;; ------------------------------------------------------------------------
  ;; 数値フォーマット補助関数
  ;; ------------------------------------------------------------------------
  (defun formatVal (val dec /)
    (if (equal val (float (fix (abs val))) 1e-6)
      (rtos val 2 0)
      (rtos val 2 dec)
    )
  )

  ;; ------------------------------------------------------------------------
  ;; 寸法端点に接続されている線分（LINE）を連動ストレッチする補助関数
  ;; ------------------------------------------------------------------------
  (defun stretchConnectedLines (pt vec / tol pMin pMax ssNear j lEnt lData pStart pEnd modified)
    (setq tol 0.05) ; 端点一致判定の許容誤差
    (setq modified 0)
    ;; pt 周辺の微小範囲で交差する LINE を検索
    (setq pMin (list (- (car pt) 0.5) (- (cadr pt) 0.5) (- (caddr pt) 0.5)))
    (setq pMax (list (+ (car pt) 0.5) (+ (cadr pt) 0.5) (+ (caddr pt) 0.5)))
    (if (setq ssNear (ssget "_C" pMin pMax '((0 . "LINE"))))
      (repeat (setq j (sslength ssNear))
        (setq lEnt (ssname ssNear (setq j (1- j))))
        (setq lData (entget lEnt))
        (setq pStart (cdr (assoc 10 lData)))
        (setq pEnd   (cdr (assoc 11 lData)))

        ;; 始点(10)が寸法定義点と一致する場合、移動ベクトルを加算
        (if (equal (distance pt pStart) 0.0 tol)
          (progn
            (setq lData (subst (cons 10 (mapcar '+ pStart vec)) (assoc 10 lData) lData))
            (entmod lData)
            (entupd lEnt)
            (setq modified (1+ modified))
          )
        )
        ;; 終点(11)が寸法定義点と一致する場合、移動ベクトルを加算
        (if (equal (distance pt pEnd) 0.0 tol)
          (progn
            (setq lData (subst (cons 11 (mapcar '+ pEnd vec)) (assoc 11 lData) lData))
            (entmod lData)
            (entupd lEnt)
            (setq modified (1+ modified))
          )
        )
      )
    )
    modified
  )

  ;; ------------------------------------------------------------------------
  ;; 初期化
  ;; ------------------------------------------------------------------------
  (setq oldCmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq dimDec (getvar "DIMDEC"))

  ;; ------------------------------------------------------------------------
  ;; 1. 片側の拡張量を入力（デフォルト値・前回値を表示）
  ;; ------------------------------------------------------------------------
  (initget 0) ; 正・負・ゼロ入力可能
  (setq userInc 
    (getreal 
      (strcat "\n片側の拡張量を指定（両端で計2倍拡張） <" (formatVal *STRETCH_DIM_INC* dimDec) ">: ")
    )
  )
  (if userInc
    (setq *STRETCH_DIM_INC* userInc)
  )
  (setq incVal *STRETCH_DIM_INC*)

  ;; ------------------------------------------------------------------------
  ;; 2. 長さ寸法の選択（複数選択対応）
  ;; ------------------------------------------------------------------------
  (prompt "\n両端を拡張する長さ寸法を選択: ")
  (setq ss (ssget '((0 . "DIMENSION"))))

  (if (not ss)
    (progn
      (princ "\n寸法オブジェクトが選択されませんでした。")
      (setvar "CMDECHO" oldCmd)
      (princ)
      (exit)
    )
  )

  ;; ------------------------------------------------------------------------
  ;; 3. 寸法および接続形状の両端ストレッチ処理
  ;; ------------------------------------------------------------------------
  (vla-StartUndoMark doc) ; Undoグループ開始
  (setq count 0)
  (setq lineCount 0)

  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (setq elist (entget ent))
    ;; DXF 70 の下位3ビットで寸法タイプを判定
    ;; 0 = 回転寸法 (水平・垂直・回転)
    ;; 1 = 平行寸法 (Aligned)
    (setq dimType (logand (cdr (assoc 70 elist)) 7))

    (cond
      ;; --------------------------------------------------------------------
      ;; A. 平行寸法 (Aligned Dimension: dimType = 1)
      ;; --------------------------------------------------------------------
      ((= dimType 1)
       (setq p1 (cdr (assoc 13 elist))) ; 第1定義点 (WCS)
       (setq p2 (cdr (assoc 14 elist))) ; 第2定義点 (WCS)
       (setq dx (- (car p2) (car p1)))
       (setq dy (- (cadr p2) (cadr p1)))
       (setq dz (- (caddr p2) (caddr p1)))
       (setq dist (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))

       (if (> dist 1e-6)
         (progn
           ;; 単位ベクトル (P1 -> P2 方向)
           (setq ux (/ dx dist))
           (setq uy (/ dy dist))
           (setq uz (/ dz dist))

           ;; P1の移動ベクトル (外側 = P2と反対方向 = -u)
           (setq v1 (list (* (- incVal) ux) (* (- incVal) uy) (* (- incVal) uz)))
           ;; P2の移動ベクトル (外側 = P2方向 = +u)
           (setq v2 (list (* incVal ux) (* incVal uy) (* incVal uz)))

           ;; 新しい定義点座標を計算
           (setq newP1 (mapcar '+ p1 v1))
           (setq newP2 (mapcar '+ p2 v2))

           ;; DXF 13, 14 を更新
           (setq elist (subst (cons 13 newP1) (assoc 13 elist) elist))
           (setq elist (subst (cons 14 newP2) (assoc 14 elist) elist))

           ;; 固定文字上書きがあればクリアして自動寸法値（実測値）を表示
           (setq curText (cdr (assoc 1 elist)))
           (if (and curText (/= curText "") (not (vl-string-search "<>" curText)))
             (setq elist (subst (cons 1 "") (assoc 1 elist) elist))
           )

           ;; 寸法ブロック名(DXF 2)を除去してブロック再生成を促す
           (if (assoc 2 elist)
             (setq elist (vl-remove (assoc 2 elist) elist))
           )

           (entmod elist)
           (entupd ent)

           ;; 接続されている線分（LINE）があれば連動ストレッチ
           (setq lineCount (+ lineCount (stretchConnectedLines p1 v1)))
           (setq lineCount (+ lineCount (stretchConnectedLines p2 v2)))

           (setq count (1+ count))
         )
       )
      )

      ;; --------------------------------------------------------------------
      ;; B. 回転寸法 (Rotated Dimension: dimType = 0, 水平・垂直・斜め回転寸法)
      ;; --------------------------------------------------------------------
      ((= dimType 0)
       (setq p1 (cdr (assoc 13 elist))) ; 第1定義点 (WCS)
       (setq p2 (cdr (assoc 14 elist))) ; 第2定義点 (WCS)
       (setq rot (cdr (assoc 50 elist))) ; 寸法回転角度（ラジアン）
       (if (null rot) (setq rot 0.0))

       (setq cosA (cos rot))
       (setq sinA (sin rot))

       ;; 測定軸への射影ベクトル計算
       (setq dx (- (car p2) (car p1)))
       (setq dy (- (cadr p2) (cadr p1)))
       (setq dot (+ (* dx cosA) (* dy sinA)))

       ;; dot の符号によって外側方向（P1 -> P2 の向き）を決定
       (setq dir (if (< dot 0.0) -1.0 1.0))
       (setq vOut (list (* dir cosA) (* dir sinA) 0.0))

       ;; P1の移動ベクトル (外側 = -vOut)
       (setq v1 (list (* (- incVal) (car vOut)) (* (- incVal) (cadr vOut)) 0.0))
       ;; P2の移動ベクトル (外側 = +vOut)
       (setq v2 (list (* incVal (car vOut)) (* incVal (cadr vOut)) 0.0))

       ;; 新しい定義点座標を計算
       (setq newP1 (mapcar '+ p1 v1))
       (setq newP2 (mapcar '+ p2 v2))

       ;; DXF 13, 14 を更新
       (setq elist (subst (cons 13 newP1) (assoc 13 elist) elist))
       (setq elist (subst (cons 14 newP2) (assoc 14 elist) elist))

       ;; 固定文字上書きがあればクリアして自動寸法値（実測値）を表示
       (setq curText (cdr (assoc 1 elist)))
       (if (and curText (/= curText "") (not (vl-string-search "<>" curText)))
         (setq elist (subst (cons 1 "") (assoc 1 elist) elist))
       )

       ;; 寸法ブロック名(DXF 2)を除去してブロック再生成を促す
       (if (assoc 2 elist)
         (setq elist (vl-remove (assoc 2 elist) elist))
       )

       (entmod elist)
       (entupd ent)

       ;; 接続されている線分（LINE）があれば連動ストレッチ
       (setq lineCount (+ lineCount (stretchConnectedLines p1 v1)))
       (setq lineCount (+ lineCount (stretchConnectedLines p2 v2)))

       (setq count (1+ count))
      )
    )
  )

  (vla-EndUndoMark doc) ; Undoグループ終了
  (setvar "CMDECHO" oldCmd)

  ;; ------------------------------------------------------------------------
  ;; 4. 処理結果の出力
  ;; ------------------------------------------------------------------------
  (princ 
    (strcat "\n[STRETCH_DIM_BOTH] " (itoa count) " 個の寸法を両端ストレッチしました。"
            "\n  - 片側の拡張量 : " (formatVal incVal dimDec) 
            "\n  - 合計の延長量 : " (formatVal (* 2.0 incVal) dimDec)
            (if (> lineCount 0)
              (strcat "\n  - 連動更新線分 : " (itoa lineCount) " 箇所")
              ""
            )
    )
  )
  (princ)
)

;; 短縮コマンド登録: SDB
(defun c:SDB () (c:STRETCH_DIM_BOTH))

;; ロード完了メッセージ
(princ "\n=======================================================")
(princ "\n[STRETCH_DIM_BOTH] (短縮コマンド: SDB) がロードされました。")
(princ "\n両端をそれぞれ指定値ずつ外側へ広げて寸法＆線分をストレッチします。")
(princ "\n=======================================================\n")
(princ)