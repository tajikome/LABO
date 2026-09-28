;;; ============================================================
;;; EREC コマンド - 四角形・正方形作成（モード先行選択版）
;;;
;;; モード選択:
;;;   数値 Enter       → X幅として確定 → 基点 → Y高さ入力 → 完成
;;;   S Enter          → 正方形モード → 基点 → 一辺を入力 → 完成
;;;   C Enter          → クリックモード → 基点 → 対角点クリック → 完成
;;;   空 Enter (直Enter) → 通常モード   → 基点 → X幅入力 → Y高さ入力 → 完成
;;; ============================================================

(defun C:EREC (/ pt1 pt2 width height size mode result)

  ;; ---- 1. 入力モード選択（先行指定） ----
  (initget "S C")
  (setq result (getreal "\nX方向(幅)を入力 または [S=正方形 / C=クリックモード / Enter=通常モード]: "))

  (cond

    ;; ----------------------------------------------------
    ;; A. S が入力された場合：正方形モード
    ;; ----------------------------------------------------
    ((= result "S")
     (setq pt1 (getpoint "\n基点を指定: "))
     (if (null pt1) (exit))
     (setq pt1 (list (car pt1) (cadr pt1) 0.0))

     (initget 6) ; ゼロ・負数禁止
     (setq size (getreal "\n正方形の一辺の長さを入力: "))
     (if (null size) (exit))

     (setq pt2 (list (+ (car pt1) size) (+ (cadr pt1) size) 0.0))
     (command "_.RECTANG" pt1 pt2)
     (princ (strcat "\n作成完了(正方形): 一辺=" (rtos size 2 0) "mm"))
    )

    ;; ----------------------------------------------------
    ;; B. C が入力された場合：クリックモード
    ;; ----------------------------------------------------
    ((= result "C")
     (setq pt1 (getpoint "\n基点を指定: "))
     (if (null pt1) (exit))
     (setq pt1 (list (car pt1) (cadr pt1) 0.0))

     (setq pt2 (getcorner pt1 "\n対角点をクリック（スナップ有効）: "))
     (if (null pt2) (exit))
     (setq pt2 (list (car pt2) (cadr pt2) 0.0))

     (command "_.RECTANG" pt1 pt2)
     (princ
       (strcat "\n作成完了: 幅="
               (rtos (abs (- (car pt2) (car pt1))) 2 0) "mm"
               "  高さ="
               (rtos (abs (- (cadr pt2) (cadr pt1))) 2 0) "mm"))
    )

    ;; ----------------------------------------------------
    ;; C. 空Enter（入力なし）の場合：通常モード
    ;;    （基点指定 → X幅入力 → Y高さ入力）
    ;; ----------------------------------------------------
    ((null result)
     (setq pt1 (getpoint "\n基点を指定: "))
     (if (null pt1) (exit))
     (setq pt1 (list (car pt1) (cadr pt1) 0.0))

     (initget 6)
     (setq width (getreal "\nX方向(幅)を入力: "))
     (if (null width) (exit))

     (initget 6)
     (setq height (getreal (strcat "\nY方向(高さ)を入力 [幅=" (rtos width 2 0) "mm]: ")))
     (if (null height) (exit))

     (setq pt2 (list (+ (car pt1) width) (+ (cadr pt1) height) 0.0))
     (command "_.RECTANG" pt1 pt2)
     (princ (strcat "\n作成完了: 幅=" (rtos width 2 0) "mm  高さ=" (rtos height 2 0) "mm"))
    )

    ;; ----------------------------------------------------
    ;; D. 最初から「数値」が入力された場合
    ;;    （X幅決定済 → 基点指定 → Y高さ入力）
    ;; ----------------------------------------------------
    (T
     (setq width result)
     (if (<= width 0)
       (progn (princ "\n無効な値です。") (exit))
     )

     (setq pt1 (getpoint "\n基点を指定: "))
     (if (null pt1) (exit))
     (setq pt1 (list (car pt1) (cadr pt1) 0.0))

     (initget 6)
     (setq height (getreal (strcat "\nY方向(高さ)を入力 [幅=" (rtos width 2 0) "mm]: ")))
     (if (null height) (exit))

     (setq pt2 (list (+ (car pt1) width) (+ (cadr pt1) height) 0.0))
     (command "_.RECTANG" pt1 pt2)
     (princ (strcat "\n作成完了: 幅=" (rtos width 2 0) "mm  高さ=" (rtos height 2 0) "mm"))
    )

  )

  (princ)
)

(princ "\n[EREC] 四角形・正方形作成（先行入力版） - ロード完了")
(princ "\n  空Enter  : Enter → 基点 → X幅Enter → Y高さEnter")
(princ "\n  幅先行   : X幅Enter → 基点 → Y高さEnter")
(princ "\n  正方形   : S Enter → 基点 → 一辺の長さEnter")
(princ "\n  クリック : C Enter → 基点 → 対角点クリック")
(princ)