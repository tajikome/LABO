;;; =========================================================================
;;; コマンド名: RIN
;;; 概要: 部屋の内側をクリックするだけで、境界を自動検出して内寸の寸法線を
;;;       部屋の内側に自動作図します。(BOOの境界作成 + ALL-IN-DIMの寸法作図)
;;; -------------------------------------------------------------------------
;;; 【主な機能】
;;;  1. 壁で囲まれた内側をクリック → 境界ポリラインを自動作成(-BOUNDARY)
;;;  2. 同一直線上の余分な頂点を自動で統合(BOOでの頂点削除作業が不要)
;;;  3. 柱などの島(内側の閉じた図形)は無視し、最外周の内寸のみを作図
;;;  4. 寸法線は必ず部屋の内側に配置(ポリラインの向きに依存しない)
;;;  5. モード: [全辺] 全ての辺に作図 / [縦横] 最長の水平辺・垂直辺のみ
;;;  6. 作成した境界ポリラインは自動削除(オプションで残せます)
;;;  7. 連続して複数の部屋を処理可能(Enterで終了)
;;;  8. 取り消し: 終了後は Ctrl+Z(UNDO)で1回のクリック分ずつ戻せます。
;;;     コマンド実行中はオプション(U)で直前の部屋の寸法を取り消せます。
;;; -------------------------------------------------------------------------
;;; 【注意】
;;;  ・寸法スタイルは現在のもの、画層は現在の画層で作図されます。
;;;  ・円弧の辺は寸法を作図しません(件数を表示します)。
;;;  ・壁に隙間があって境界が作れない場合は、クリック位置から上下左右の壁までを
;;;    直接測定し、壁が見つかった方向(両側が揃った軸)だけ寸法を作図します。
;;;    隙間側の先に別の壁(隣室など)があると、それを拾うことがあります。
;;;    測定値を表示するので確認してください。探索距離はオプション(S)で変更可能。
;;;  ・隙間の場所自体を確認したい場合は BOO を使ってください。
;;; =========================================================================

;;; --- 設定の初期値(再ロードしても前回値を保持) ---
(if (null *RIN-OFFSET*) (setq *RIN-OFFSET* 150.0)) ; 壁から寸法線までの距離
(if (null *RIN-MODE*)   (setq *RIN-MODE* "All"))   ; "All"=全辺 / "Rect"=縦横のみ
(if (null *RIN-MAXD*)   (setq *RIN-MAXD* 15000.0)) ; 隙間時の壁探索距離
;; *RIN-KEEP* : T なら境界ポリラインを残す(初期値 nil=削除)

;;; -------------------------------------------------------------------------
;;; 補助関数: LWPOLYLINEのDXFデータから (座標 バルジ) のリストを取得
;;; -------------------------------------------------------------------------
(defun RIN:get-verts (eg / verts)
  (foreach it eg
    (cond
      ((= (car it) 10)
       (setq verts (cons (list (list (cadr it) (caddr it)) 0.0) verts)))
      ((and (= (car it) 42) verts)
       (setq verts (cons (list (car (car verts)) (cdr it)) (cdr verts))))
    )
  )
  (reverse verts)
)

;;; -------------------------------------------------------------------------
;;; 補助関数: 前後の頂点と同一直線上(直線辺のみ)かどうか
;;; -------------------------------------------------------------------------
(defun RIN:collinear-p (prv cur nxt tol / p0 p1 p2 v1x v1y v2x v2y cr dt d)
  (and
    (< (abs (cadr prv)) 1e-9)
    (< (abs (cadr cur)) 1e-9)
    (progn
      (setq p0 (car prv) p1 (car cur) p2 (car nxt))
      (setq v1x (- (car p1) (car p0))  v1y (- (cadr p1) (cadr p0))
            v2x (- (car p2) (car p1))  v2y (- (cadr p2) (cadr p1)))
      (setq cr (- (* v1x v2y) (* v1y v2x))
            dt (+ (* v1x v2x) (* v1y v2y))
            d  (distance p0 p2))
      (and (> d 1e-9) (>= dt -1e-9) (< (/ (abs cr) d) tol))
    )
  )
)

;;; -------------------------------------------------------------------------
;;; 補助関数: 同一直線上の余分な頂点を除去して「角」の頂点だけにする
;;; -------------------------------------------------------------------------
(defun RIN:reduce (verts tol / n i out)
  (setq n (length verts) i 0)
  (while (< i n)
    (if (not (RIN:collinear-p (nth (rem (+ i n -1) n) verts)
                              (nth i verts)
                              (nth (rem (1+ i) n) verts)
                              tol))
      (setq out (cons (nth i verts) out))
    )
    (setq i (1+ i))
  )
  (reverse out)
)

;;; -------------------------------------------------------------------------
;;; 補助関数: 符号付き面積(正=反時計回り)
;;; -------------------------------------------------------------------------
(defun RIN:signed-area (verts / n i a p q)
  (setq n (length verts) i 0 a 0.0)
  (while (< i n)
    (setq p (car (nth i verts))
          q (car (nth (rem (1+ i) n) verts)))
    (setq a (+ a (- (* (car p) (cadr q)) (* (car q) (cadr p)))))
    (setq i (1+ i))
  )
  (/ a 2.0)
)

;;; -------------------------------------------------------------------------
;;; 補助関数: 1辺の整列寸法を内側に作図
;;; -------------------------------------------------------------------------
(defun RIN:dim-edge (ent elev p1 p2 ccw / ang mid nrm dp u1 u2 u3)
  (setq ang (angle p1 p2)
        mid (list (/ (+ (car p1) (car p2)) 2.0)
                  (/ (+ (cadr p1) (cadr p2)) 2.0))
        nrm (if ccw (+ ang (/ pi 2.0)) (- ang (/ pi 2.0)))
        dp  (polar mid nrm *RIN-OFFSET*))
  ;; OCS → 現在のUCS へ変換
  (setq u1 (trans (list (car p1) (cadr p1) elev) ent 1)
        u2 (trans (list (car p2) (cadr p2) elev) ent 1)
        u3 (trans (list (car dp) (cadr dp) elev) ent 1))
  (command "_.dimaligned" "_non" u1 "_non" u2 "_non" u3)
)

;;; -------------------------------------------------------------------------
;;; 補助関数: 指定点から指定角度(UCS)方向へ最初に当たる壁の交点を返す(なければnil)
;;; -------------------------------------------------------------------------
(defun RIN:ray-hit (pt ang maxd / far ss i obj ray rayObj res lst tmp d best bestD)
  (setq far (polar pt ang maxd))
  (setq ss (ssget "_F" (list pt far)
                  '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE,SPLINE"))))
  (if ss
    (progn
      ;; 一時的な光線用LINE(WCS)を作成して交点を求める
      (setq ray (entmakex (list '(0 . "LINE")
                                (cons 10 (trans pt 1 0))
                                (cons 11 (trans far 1 0)))))
      (if ray
        (progn
          (setq rayObj (vlax-ename->vla-object ray))
          (setq i 0)
          (while (< i (sslength ss))
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (setq res (vl-catch-all-apply 'vlax-invoke (list obj 'IntersectWith rayObj 0)))
            (if (and res (not (vl-catch-all-error-p res)))
              (progn
                (setq lst res)
                (while (>= (length lst) 3)
                  (setq tmp (trans (list (car lst) (cadr lst) (caddr lst)) 0 1))
                  (setq d (distance (list (car pt) (cadr pt)) (list (car tmp) (cadr tmp))))
                  (if (and (> d 1e-6) (or (null bestD) (< d bestD)))
                    (setq bestD d
                          best (list (car tmp) (cadr tmp) (caddr pt)))
                  )
                  (setq lst (cdddr lst))
                )
              )
            )
            (setq i (1+ i))
          )
          (entdel ray)
        )
      )
    )
  )
  best
)

;;; -------------------------------------------------------------------------
;;; 隙間がある場合の処理: クリック位置から上下左右の壁までを測って寸法作図
;;; -------------------------------------------------------------------------
(defun RIN:room-ray (pt / hR hL hU hD cnt)
  (setq cnt 0)
  (setq hR (RIN:ray-hit pt 0.0 *RIN-MAXD*)
        hL (RIN:ray-hit pt pi *RIN-MAXD*)
        hU (RIN:ray-hit pt (/ pi 2.0) *RIN-MAXD*)
        hD (RIN:ray-hit pt (* pi 1.5) *RIN-MAXD*))

  ;; 横方向(左右の壁が両方見つかった場合のみ)
  (if (and hL hR)
    (progn
      (command "_.dimaligned" "_non" hL "_non" hR "_non"
               (list (/ (+ (car hL) (car hR)) 2.0) (+ (cadr pt) *RIN-OFFSET*) (caddr pt)))
      (setq cnt (1+ cnt))
      (princ (strcat "
  横: " (rtos (distance hL hR) 2 0)))
    )
    (princ (strcat "
  横: 作図しません(壁が見つからない方向: "
                   (if hL "" "左 ") (if hR "" "右") ")"))
  )

  ;; 縦方向(上下の壁が両方見つかった場合のみ)
  (if (and hD hU)
    (progn
      (command "_.dimaligned" "_non" hD "_non" hU "_non"
               (list (+ (car pt) *RIN-OFFSET*) (/ (+ (cadr hD) (cadr hU)) 2.0) (caddr pt)))
      (setq cnt (1+ cnt))
      (princ (strcat "
  縦: " (rtos (distance hD hU) 2 0)))
    )
    (princ (strcat "
  縦: 作図しません(壁が見つからない方向: "
                   (if hD "" "下 ") (if hU "" "上") ")"))
  )
  cnt
)

;;; -------------------------------------------------------------------------
;;; 本体: 指定点の部屋について境界作成 → 内寸作図。作図した寸法数を返す
;;; -------------------------------------------------------------------------
(defun RIN:room (pt / lastEnt e typ cand best bestA a eg elev verts ccw
                      n i v1 v2 edges arcCnt hEd vEd hLen vLen dx dy len sel cnt)
  (setq cnt 0)

  ;; 1. 境界の自動作成
  (setq lastEnt (entlast))
  (command "_.-boundary" pt "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))

  ;; 2. 新しく作られたポリラインを収集
  (setq e (if lastEnt (entnext lastEnt) (entnext)))
  (while e
    (setq typ (cdr (assoc 0 (entget e))))
    (if (member typ '("LWPOLYLINE" "POLYLINE"))
      (setq cand (cons e cand))
    )
    (setq e (entnext e))
  )

  (if (null cand)
    (progn
      (princ "\n[RIN] 境界を作成できませんでした(壁に隙間の可能性)。クリック位置から上下左右の壁までを直接測定します。")
      (setq cnt (RIN:room-ray pt))
    )
    (progn
      ;; 3. 最も面積の大きいもの(=最外周)を採用し、島は削除
      (foreach c cand
        (setq a (vl-catch-all-apply 'vla-get-area (list (vlax-ename->vla-object c))))
        (if (and (numberp a) (or (null best) (> a bestA)))
          (setq best c bestA a)
        )
      )
      (if (null best) (setq best (car cand)))
      (foreach c cand (if (not (eq c best)) (entdel c)))

      ;; 旧形式ポリラインは軽量ポリラインへ変換
      (if (/= (cdr (assoc 0 (entget best))) "LWPOLYLINE")
        (progn
          (command "_.convertpoly" "_L" best "")
          (setq best (entlast))
        )
      )

      (if (/= (cdr (assoc 0 (entget best))) "LWPOLYLINE")
        (progn
          (princ "\n[RIN] 境界をポリラインとして取得できませんでした。")
          (entdel best)
        )
        (progn
          ;; 4. 頂点取得 → 余分な頂点を統合
          (setq eg    (entget best)
                elev  (if (assoc 38 eg) (cdr (assoc 38 eg)) 0.0)
                verts (RIN:reduce (RIN:get-verts eg) 0.1))

          (if (< (length verts) 3)
            (princ "\n[RIN] 有効な頂点が足りないため作図できませんでした。")
            (progn
              (setq ccw (> (RIN:signed-area verts) 0.0))

              ;; 5. 辺リストを作成 (始点 終点 バルジ)
              (setq n (length verts) i 0)
              (while (< i n)
                (setq v1 (nth i verts)
                      v2 (nth (rem (1+ i) n) verts))
                (setq edges (cons (list (car v1) (car v2) (cadr v1)) edges))
                (setq i (1+ i))
              )
              (setq edges (reverse edges))

              ;; 円弧の辺と極小の辺は対象外
              (setq arcCnt 0)
              (foreach ed edges
                (if (> (abs (caddr ed)) 1e-9) (setq arcCnt (1+ arcCnt)))
              )
              (setq edges
                (vl-remove-if
                  '(lambda (ed)
                     (or (> (abs (caddr ed)) 1e-9)
                         (< (distance (car ed) (cadr ed)) 1.0)))
                  edges))

              ;; 6. モードに応じて作図対象を決定
              (if (= *RIN-MODE* "Rect")
                (progn
                  (setq hLen 0.0 vLen 0.0 hEd nil vEd nil)
                  (foreach ed edges
                    (setq dx  (abs (- (car (cadr ed)) (car (car ed))))
                          dy  (abs (- (cadr (cadr ed)) (cadr (car ed))))
                          len (distance (car ed) (cadr ed)))
                    (if (and (< dy (* len 0.001)) (> len hLen))
                      (setq hEd ed hLen len))
                    (if (and (< dx (* len 0.001)) (> len vLen))
                      (setq vEd ed vLen len))
                  )
                  (setq sel (vl-remove nil (list hEd vEd)))
                )
                (setq sel edges)
              )

              ;; 7. 寸法作図
              (foreach ed sel
                (RIN:dim-edge best elev (car ed) (cadr ed) ccw)
                (setq cnt (1+ cnt))
              )

              (if (> arcCnt 0)
                (princ (strcat "\n[RIN] 円弧の辺 " (itoa arcCnt) " 箇所は寸法を作図していません。"))
              )
            )
          )

          ;; 8. 境界ポリラインの後始末
          (if (and (not *RIN-KEEP*) (entget best))
            (entdel best)
          )
        )
      )
    )
  )
  cnt
)

;;; -------------------------------------------------------------------------
;;; メインコマンド
;;; -------------------------------------------------------------------------
(defun c:RIN ( / *error* oldCmdecho oldOsmode undoOpen loopFlag pt cnt total d m
                 history lastBefore newEnts e)
  (vl-load-com)

  (defun *error* (msg)
    (repeat 3 (if (> (getvar "CMDACTIVE") 0) (command)))
    (if undoOpen (command "_.undo" "_end"))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\nエラー: " msg))
    )
    (princ "\n[RIN] 中断しました。設定を復元しました。")
    (princ)
  )

  (setq oldCmdecho (getvar "CMDECHO")
        oldOsmode  (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  (princ "\n--- 部屋内寸 自動作図 (RIN) ---")
  (princ (strcat "\n現在の設定: 寸法線位置=" (rtos *RIN-OFFSET* 2 1)
                 " / モード=" (if (= *RIN-MODE* "Rect") "縦横のみ" "全辺")
                 " / 境界ポリライン=" (if *RIN-KEEP* "残す" "削除")
                 " / 隙間時の探索距離=" (rtos *RIN-MAXD* 2 0)))

  (setq loopFlag T total 0 history nil)
  (while loopFlag
    (initget "Offset Mode Keep Search Undo")
    (setq pt (getpoint "\n部屋の内側をクリック [元に戻す(U)/寸法線位置(O)/モード(M)/境界を残す(K)/探索距離(S)] <終了:Enter>: "))
    (cond
      ;; Enter → 終了
      ((null pt) (setq loopFlag nil))

      ;; キーワード
      ((= (type pt) 'STR)
       (cond
         ((= pt "Offset")
          (initget 6)
          (setq d (getdist (strcat "\n壁から寸法線までの距離 <" (rtos *RIN-OFFSET* 2 1) ">: ")))
          (if d (setq *RIN-OFFSET* d))
         )
         ((= pt "Mode")
          (initget "All Rect")
          (setq m (getkword (strcat "\nモード [全辺(All)/縦横の最長辺のみ(Rect)] <"
                                    (if (= *RIN-MODE* "Rect") "Rect" "All") ">: ")))
          (if m (setq *RIN-MODE* m))
         )
         ((= pt "Keep")
          (setq *RIN-KEEP* (not *RIN-KEEP*))
          (princ (strcat "\n境界ポリラインを " (if *RIN-KEEP* "残します。" "削除します。")))
         )
         ((= pt "Undo")
          (if history
            (progn
              (foreach e (car history)
                (if (not (vlax-erased-p (vlax-ename->vla-object e)))
                  (entdel e)
                )
              )
              (setq history (cdr history))
              (princ "\n直前の部屋の寸法を取り消しました。")
            )
            (princ "\n取り消せる作図がありません。")
          )
         )
         ((= pt "Search")
          (initget 6)
          (setq d (getdist (strcat "\n隙間がある場合の壁の探索距離 <" (rtos *RIN-MAXD* 2 0) ">: ")))
          (if d (setq *RIN-MAXD* d))
         )
       )
      )

      ;; 点 → 部屋の内寸を作図
      ((= (type pt) 'LIST)
       (setq lastBefore (entlast))
       (setq undoOpen T)
       (command "_.undo" "_begin")
       (setq cnt (RIN:room pt))
       (command "_.undo" "_end")
       (setq undoOpen nil)
       ;; 今回作成された図形を履歴に記録(U用)
       (setq newEnts nil
             e (if lastBefore (entnext lastBefore) (entnext)))
       (while e
         (if (not (vlax-erased-p (vlax-ename->vla-object e)))
           (setq newEnts (cons e newEnts))
         )
         (setq e (entnext e))
       )
       (if newEnts (setq history (cons newEnts history)))
       (setq total (+ total cnt))
       (if (> cnt 0)
         (princ (strcat "\n[RIN] " (itoa cnt) " 箇所の内寸を作図しました。"))
       )
      )
    )
  )

  (setvar "CMDECHO" oldCmdecho)
  (setvar "OSMODE" oldOsmode)
  (princ (strcat "\n[RIN] 終了しました。(合計 " (itoa total) " 箇所)"))
  (princ)
)

;;; ロード時メッセージ
(princ "\n==================================================")
(princ "\n[RIN] 部屋内寸 自動作図ツール がロードされました。")
(princ "\n  ・実行コマンド: RIN")
(princ "\n==================================================")
(princ)