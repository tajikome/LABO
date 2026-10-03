;;; ============================================================
;;; BD : ブロックのサイズ(幅・奥行き)を計測 / サイズ指定で検索
;;;  - 幅・奥行きを指定 → 図面全体(現在の空間)から該当ブロックを選択
;;;  - 指定なし(両方Enter) → 検索範囲を指定(標準=図面全体)して寸法一覧を表示
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
;;; 補助関数
;;; ============================================================

;; --- 図面全体(現在のモデル/レイアウト)のブロック挿入図形を取得 ---
(defun bld_select_all ()
  (ssget "X" (list '(0 . "INSERT") (cons 410 (getvar "CTAB"))))
)

;; --- 検索範囲を指定してブロックを取得(標準 = 図面全体) ---
(defun bld_select_range ( / opt ent ptList sset)
  (initget "Window Boundary All")
  (setq opt (getkword "\n検索範囲を指定 [窓・通常選択(W)/閉じたオブジェクト境界(B)/図面全体(A)] <図面全体>: "))
  (cond
    ((= opt "Window")
     (princ "\n対象のブロックを窓選択またはクリックで選択してください:")
     (setq sset (ssget '((0 . "INSERT"))))
    )
    ((= opt "Boundary")
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
    (T (setq sset (bld_select_all)))   ; Enter または A = 図面全体
  )
  sset
)

;; --- 選択セットをブロック名別に集計して寸法一覧を表示 ---
(defun bld_print_list (sset / lst i nm cnt item dim)
  (setq lst nil i 0)
  (while (< i (sslength sset))
    (setq nm (cdr (assoc 2 (entget (ssname sset i)))))
    (if (setq cnt (assoc nm lst))
      (setq lst (subst (cons nm (1+ (cdr cnt))) cnt lst))
      (setq lst (cons (cons nm 1) lst))
    )
    (setq i (1+ i))
  )
  (foreach item (reverse lst)
    (setq dim (bld_block_size (car item)))
    (princ (strcat "\n  ・[ブロック名: " (car item)
                   "] 幅=" (rtos (car dim) 2 2)
                   " / 奥行き=" (rtos (cadr dim) 2 2)
                   " (個数: " (itoa (cdr item)) "個)"))
  )
)

;;; ============================================================
;;; メインコマンド
;;; ============================================================
(defun c:BD ( / w d sset final-ss i e nm dim cache c)
  (vl-load-com)

  (princ "\n※ 幅・奥行きを指定すると、図面全体から該当サイズのブロックを検索します。")
  (princ "\n   両方スキップ(Enter)すると、検索範囲の指定に進みます。")
  (setq w (getreal "\n検索する「幅」を入力 [スキップはEnter]: "))
  (setq d (getreal "\n検索する「奥行き」を入力 [スキップはEnter]: "))

  (if (or w d)
    ;; ---- 幅・奥行き指定あり: 図面全体から検索 ----
    (progn
      (setq sset (bld_select_all))
      (if (or (null sset) (= (sslength sset) 0))
        (princ "\n図面内にブロックがありません。")
        (progn
          (setq final-ss (ssadd) cache nil i 0)
          (while (< i (sslength sset))
            (setq e (ssname sset i))
            (setq nm (cdr (assoc 2 (entget e))))
            ;; ブロック名ごとにサイズをキャッシュ
            (if (setq c (assoc nm cache))
              (setq dim (cdr c))
              (progn
                (setq dim (bld_block_size nm))
                (setq cache (cons (cons nm dim) cache))
              )
            )
            (if (and (or (null w) (< (abs (- (car dim) w)) 0.001))
                     (or (null d) (< (abs (- (cadr dim) d)) 0.001)))
              (ssadd e final-ss)
            )
            (setq i (1+ i))
          )
          (if (> (sslength final-ss) 0)
            (progn
              (sssetfirst nil final-ss)
              (princ "\n\n=== 【条件に一致したブロック】 ===")
              (bld_print_list final-ss)
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
    ;; ---- 指定なし: 検索範囲を指定して寸法一覧を表示 ----
    (progn
      (setq sset (bld_select_range))
      (if (or (null sset) (= (sslength sset) 0))
        (princ "\n\nブロックが見つかりませんでした。")
        (progn
          (sssetfirst nil sset)
          (princ "\n\n=== 【ブロックの計測結果一覧】 ===")
          (bld_print_list sset)
          (princ (strcat "\n--- 処理完了: 合計 " (itoa (sslength sset)) " 個のブロックを選択状態にしています ---"))
        )
      )
    )
  )
  (princ)
)

(princ "\n[BD] コマンドがロードされました。コマンド名: BD")
(princ)