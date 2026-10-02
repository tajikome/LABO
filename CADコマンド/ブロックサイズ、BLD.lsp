;;; ============================================================
;;; BLD : 選択したブロックのサイズ(幅・奥行き)を計測し、サイズで絞り込み選択
;;;  - 対応図形 : LINE / LWPOLYLINE(円弧セグメント=バルジ対応) / CIRCLE / ARC
;;;  - 三角形(TRI150で描く 1辺150の閉じた3頂点ポリライン)は計測から除外
;;; ============================================================

;; 三角形(TRI150)の1辺の長さ
(setq *bld-tri-len* 150.0)

;; --- 範囲(bb = (minX minY maxX maxY))に点を追加 ---
(defun bld_add_pt (bb x y)
  (if bb
    (list (min (nth 0 bb) x) (min (nth 1 bb) y)
          (max (nth 2 bb) x) (max (nth 3 bb) y))
    (list x y x y)
  )
)

;; --- 角度を 0〜2π に正規化 ---
(defun bld_norm (a / tp)
  (setq tp (* 2.0 pi))
  (while (< a 0.0) (setq a (+ a tp)))
  (while (>= a tp) (setq a (- a tp)))
  a
)

;; --- 円弧(中心・半径・開始角・終了角 / 反時計回り)の範囲を追加 ---
(defun bld_add_arc (bb cx cy r a1 a2 / sweep a)
  (setq bb (bld_add_pt bb (+ cx (* r (cos a1))) (+ cy (* r (sin a1)))))
  (setq bb (bld_add_pt bb (+ cx (* r (cos a2))) (+ cy (* r (sin a2)))))
  (setq sweep (bld_norm (- a2 a1)))
  ;; 0°,90°,180°,270°の極点が円弧の範囲内なら追加
  (foreach a (list 0.0 (* 0.5 pi) pi (* 1.5 pi))
    (if (<= (bld_norm (- a a1)) sweep)
      (setq bb (bld_add_pt bb (+ cx (* r (cos a))) (+ cy (* r (sin a)))))
    )
  )
  bb
)

;; --- バルジ付きセグメント(p1→p2)の円弧部分の範囲を追加 ---
(defun bld_add_bulge (bb p1 p2 b / x1 y1 x2 y2 dx dy d c h r mx my cx cy)
  (setq x1 (car p1) y1 (cadr p1) x2 (car p2) y2 (cadr p2))
  (setq dx (- x2 x1) dy (- y2 y1))
  (setq d (sqrt (+ (* dx dx) (* dy dy))))
  (if (or (< d 1.0e-9) (< (abs b) 1.0e-9))
    bb
    (progn
      (setq c (/ d 2.0))
      (setq h (/ (* c (- 1.0 (* b b))) (* 2.0 b)))        ; 弦の中点から中心までの符号付き距離
      (setq r (/ (* c (+ 1.0 (* b b))) (* 2.0 (abs b))))  ; 半径
      (setq mx (/ (+ x1 x2) 2.0) my (/ (+ y1 y2) 2.0))
      (setq cx (+ mx (* h (/ (- dy) d))))
      (setq cy (+ my (* h (/ dx d))))
      (if (> b 0.0)
        (bld_add_arc bb cx cy r (atan (- y1 cy) (- x1 cx)) (atan (- y2 cy) (- x2 cx)))
        (bld_add_arc bb cx cy r (atan (- y2 cy) (- x2 cx)) (atan (- y1 cy) (- x1 cx)))
      )
    )
  )
)

;; --- LWPOLYLINEの頂点リスト ((x y bulge) ...) を取得 ---
(defun bld_lw_verts (ed / v g)
  (foreach g ed
    (cond
      ((= (car g) 10)
       (setq v (cons (list (cadr g) (caddr g) 0.0) v))
      )
      ((and (= (car g) 42) v)
       (setq v (cons (list (car (car v)) (cadr (car v)) (cdr g)) (cdr v)))
      )
    )
  )
  (reverse v)
)

;; --- 三角形(TRI150)かどうか判定 ---
;;  閉じた3頂点・円弧なし・3辺とも *bld-tri-len*
(defun bld_is_tri (ed verts / v1 v2 v3 tol)
  (setq tol 0.01)
  (and
    (= (length verts) 3)
    (= 1 (logand 1 (cond ((cdr (assoc 70 ed))) (0))))
    (< (abs (nth 2 (nth 0 verts))) 1.0e-9)
    (< (abs (nth 2 (nth 1 verts))) 1.0e-9)
    (< (abs (nth 2 (nth 2 verts))) 1.0e-9)
    (progn
      (setq v1 (nth 0 verts) v2 (nth 1 verts) v3 (nth 2 verts))
      (and
        (< (abs (- (distance v1 v2) *bld-tri-len*)) tol)
        (< (abs (- (distance v2 v3) *bld-tri-len*)) tol)
        (< (abs (- (distance v3 v1) *bld-tri-len*)) tol)
      )
    )
  )
)

;; --- ブロック定義のサイズ (幅 奥行き) を返す ---
(defun bld_block_size (bName / e ed typ bb verts n i p1 p2 b cl
                              c pt1 pt2 dx dy has-x has-y w h v)
  (setq e (tblobjname "BLOCK" bName))
  (setq bb nil has-x nil has-y nil)
  (while e
    (setq ed (entget e))
    (setq typ (cdr (assoc 0 ed)))
    (cond
      ;; 線
      ((= typ "LINE")
       (setq pt1 (cdr (assoc 10 ed)) pt2 (cdr (assoc 11 ed)))
       (setq bb (bld_add_pt bb (car pt1) (cadr pt1)))
       (setq bb (bld_add_pt bb (car pt2) (cadr pt2)))
       ;; 長さ100の水平線・垂直線の判定(従来どおり)
       (setq dx (abs (- (car pt2) (car pt1))))
       (setq dy (abs (- (cadr pt2) (cadr pt1))))
       (if (and (< dy 0.001) (< (abs (- dx 100.0)) 0.001)) (setq has-x T))
       (if (and (< dx 0.001) (< (abs (- dy 100.0)) 0.001)) (setq has-y T))
      )
      ;; 円
      ((= typ "CIRCLE")
       (setq c (cdr (assoc 10 ed)) w (cdr (assoc 40 ed)))
       (setq bb (bld_add_pt bb (- (car c) w) (- (cadr c) w)))
       (setq bb (bld_add_pt bb (+ (car c) w) (+ (cadr c) w)))
      )
      ;; 円弧
      ((= typ "ARC")
       (setq c (cdr (assoc 10 ed)))
       (setq bb (bld_add_arc bb (car c) (cadr c) (cdr (assoc 40 ed))
                             (cdr (assoc 50 ed)) (cdr (assoc 51 ed))))
      )
      ;; ポリライン(円弧セグメント含む)
      ((= typ "LWPOLYLINE")
       (setq verts (bld_lw_verts ed))
       ;; 三角形(TRI150)は計測対象外
       (if (not (bld_is_tri ed verts))
         (progn
           (setq n (length verts))
           (setq cl (= 1 (logand 1 (cond ((cdr (assoc 70 ed))) (0)))))
           (foreach v verts
             (setq bb (bld_add_pt bb (car v) (cadr v)))
           )
           ;; 各セグメントのバルジ(円弧)を反映
           (setq i 0)
           (while (< i (if cl n (1- n)))
             (setq p1 (nth i verts))
             (setq p2 (nth (rem (1+ i) n) verts))
             (setq b (nth 2 p1))
             (if (/= b 0.0)
               (setq bb (bld_add_bulge bb p1 p2 b))
             )
             (setq i (1+ i))
           )
         )
       )
      )
    )
    (setq e (entnext e))
  )
  (if bb
    (progn
      (setq w (- (nth 2 bb) (nth 0 bb)))
      (setq h (- (nth 3 bb) (nth 1 bb)))
      ;; 従来仕様: 長さ100の水平/垂直線があるブロックは奥行き-50
      (if (or has-x has-y) (setq h (- h 50.0)))
      (list w h)
    )
    (list 0.0 0.0)
  )
)

;;; ============================================================
;;; メインコマンド
;;; ============================================================
(defun c:BLD ( / opt ent ptList sset i e blkRefName dim w h
                 uniqueBlks cnt j final-ss targetSize match bw bh)
  (vl-load-com)
  
  ;; 選択方法の指定（窓選択または境界選択）
  (initget "Window Boundary")
  (setq opt (getkword "\nブロックの選択方法を指定 [窓・通常選択(W)/閉じたオブジェクト境界(B)] : "))
  
  (setq sset nil)
  (if (= opt "Boundary")
    (progn
      (setq ent (car (entsel "\n内側にあるブロックを抽出する閉じたオブジェクト（ポリライン等）を選択: ")))
      (if ent
        (progn
          (setq ptList nil)
          (foreach x (entget ent)
            (if (= (car x) 10)
              (setq ptList (append ptList (list (cdr x))))
            )
          )
          (if ptList
            (setq sset (ssget "CP" ptList '((0 . "INSERT"))))
          )
        )
      )
    )
    (progn
      (princ "\n対象のブロックを窓選択またはクリックで選択してください:")
      (setq sset (ssget '((0 . "INSERT"))))
    )
  )
  
  ;; 選択されたブロックがない場合は終了
  (if (or (null sset) (= (sslength sset) 0))
    (progn
      (princ "\n\nブロックが選択されませんでした。")
      (exit)
    )
  )

  ;; 選択されたブロックをハイライト表示
  (sssetfirst nil sset)

  ;; --- 計測結果の集計とサイズ別リストの作成 ---
  (setq uniqueBlks '())
  (setq i 0)
  (while (< i (sslength sset))
    (setq blkRefName (cdr (assoc 2 (entget (ssname sset i)))))
    (if (not (member blkRefName uniqueBlks))
      (setq uniqueBlks (cons blkRefName uniqueBlks))
    )
    (setq i (1+ i))
  )
  
  (princ "\n\n=== 【選択したブロックの計測結果一覧】 ===")
  (foreach blkRefName uniqueBlks
    (setq dim (bld_block_size blkRefName))
    (setq w (car dim))
    (setq h (cadr dim))
    
    ;; 個数の集計
    (setq j 0 cnt 0)
    (while (< j (sslength sset))
      (if (= (cdr (assoc 2 (entget (ssname sset j)))) blkRefName)
        (setq cnt (1+ cnt))
      )
      (setq j (1+ j))
    )
    
    (princ (strcat "\n  ・[ブロック名: " blkRefName "] 幅=" (rtos w 2 2) " / 奥行き=" (rtos h 2 2) " (個数: " (itoa cnt) "個)"))
  )

  ;; --- 計測後にサイズで選び直す（フィルタリング）機能 ---
  (initget "All Specific")
  (setq targetSize (getkword "\n\n計測完了。どのように選択しますか？ [すべて維持(A) / 特定のサイズだけ絞り込む(S)] : "))
  
  (if (= targetSize "Specific")
    (progn
      (princ "\n※ 絞り込みたい項目のみ数値を入力してください。指定しない項目はそのままEnterでスキップできます。")
      (setq w (getreal "\n絞り込む「幅」を入力 [スキップはEnter]: "))
      (setq h (getreal "\n絞り込む「奥行き」を入力 [スキップはEnter]: "))
      
      ;; 両方未入力の場合は警告を出して終了
      (if (and (null w) (null h))
        (progn
          (princ "\n幅も奥行きも入力されなかったため、絞り込みをキャンセルし選択を維持します。")
          (sssetfirst nil sset)
        )
        (progn
          (setq final-ss (ssadd))
          (setq j 0)
          (while (< j (sslength sset))
            (setq e (ssname sset j))
            (setq blkRefName (cdr (assoc 2 (entget e))))
            
            ;; 再度サイズを判定
            (setq dim (bld_block_size blkRefName))
            (setq bw (car dim))
            (setq bh (cadr dim))
            
            ;; 判定フラグ
            (setq match T)
            ;; 幅が入力されている場合、幅が一致するかチェック
            (if (and w (not (< (abs (- bw w)) 0.001)))
              (setq match nil)
            )
            ;; 奥行きが入力されている場合、奥行きが一致するかチェック
            (if (and h (not (< (abs (- bh h)) 0.001)))
              (setq match nil)
            )
            
            ;; 条件に合致すれば追加
            (if match
              (ssadd e final-ss)
            )
            
            (setq j (1+ j))
          )
          
          (if (> (sslength final-ss) 0)
            (progn
              (sssetfirst nil final-ss)
              (princ (strcat "\n--- 指定された条件に一致する " (itoa (sslength final-ss)) " 個のブロックを選択しました ---"))
            )
            (progn
              (sssetfirst nil nil)
              (princ "\n--- 指定された条件に一致するブロックはありませんでした ---")
            )
          )
        )
      )
    )
    ;; すべて維持する場合
    (progn
      (sssetfirst nil sset)
      (princ (strcat "\n--- 処理完了: 合計 " (itoa (sslength sset)) " 個のブロックを選択状態にしています ---"))
    )
  )
  (princ)
)

(princ "\n[BLD] コマンドがロードされました。コマンド名: BLD")
(princ)