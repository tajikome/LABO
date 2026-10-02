;;; ============================================================
;;; EREE コマンド - 正方形作成（モードなし・直感的）
;;;   基点指定 → 一辺を入力
;;; ============================================================
(defun C:EREE (/ pt1 pt2 size)
  (setq pt1 (getpoint "\n基点を指定: "))
  (if (null pt1) (exit))
  (setq pt1 (list (car pt1) (cadr pt1) 0.0))

  (initget 6) ; ゼロ・負数禁止
  (setq size (getreal "\n正方形の一辺の長さを入力: "))
  (if (null size) (exit))

  (setq pt2 (list (+ (car pt1) size) (+ (cadr pt1) size) 0.0))
  (command "_.RECTANG" pt1 pt2)
  (princ (strcat "\n作成完了(正方形): 一辺=" (rtos size 2 0) "mm"))
  (princ)
)

;;; ============================================================
;;; EREC コマンド - 四角形作成（モードなし・直感的）
;;;   基点指定 → X幅入力 → Y高さ入力
;;; ============================================================
(defun C:EREC (/ pt1 pt2 width height)
  (setq pt1 (getpoint "\n基点を指定: "))
  (if (null pt1) (exit))
  (setq pt1 (list (car pt1) (cadr pt1) 0.0))

  (initget 6) ; ゼロ・負数禁止
  (setq width (getreal "\nX方向(幅)を入力: "))
  (if (null width) (exit))

  (initget 6) ; ゼロ・負数禁止
  (setq height (getreal (strcat "\nY方向(高さ)を入力 [幅=" (rtos width 2 0) "mm]: ")))
  (if (null height) (exit))

  (setq pt2 (list (+ (car pt1) width) (+ (cadr pt1) height) 0.0))
  (command "_.RECTANG" pt1 pt2)
  (princ (strcat "\n作成完了: 幅=" (rtos width 2 0) "mm  高さ=" (rtos height 2 0) "mm"))
  (princ)
)

(princ "\n[EREC / EREE] ロード完了")
(princ "\n  EREE : 基点指定 → 一辺の長さを入力（正方形）")
(princ "\n  EREC : 基点指定 → X幅を入力 → Y高さを入力（長方形）")
(princ)