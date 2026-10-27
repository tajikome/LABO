;;; ============================================================
;;; EREC コマンド - 四角形作成（モードなし・直感的）
;;;   基点指定 → X幅入力 → Y高さ入力
;;;   高さ入力で Enter のみ押すと、幅と同じ値（正方形）になる
;;; ============================================================
(defun C:EREC (/ pt1 pt2 width height)
  (setq pt1 (getpoint "\n基点を指定: "))
  (if (null pt1) (exit))
  (setq pt1 (list (car pt1) (cadr pt1) 0.0))

  (initget 6) ; ゼロ・負数禁止
  (setq width (getreal "\nX方向(幅)を入力: "))
  (if (null width) (exit))

  (initget 6) ; ゼロ・負数禁止（Enterのみは許可）
  (setq height
         (getreal (strcat "\nY方向(高さ)を入力 <" (rtos width 2 0) ">: ")))
  (if (null height) (setq height width)) ; Enterのみ → 幅と同じ値

  (setq pt2 (list (+ (car pt1) width) (+ (cadr pt1) height) 0.0))
  (command "_.RECTANG" pt1 pt2)
  (princ (strcat "\n作成完了: 幅=" (rtos width 2 0) "mm  高さ=" (rtos height 2 0) "mm"))
  (princ)
)

(princ "\n[EREC] ロード完了")
(princ "\n  EREC : 基点指定 → X幅を入力 → Y高さを入力（Enterで幅と同じ値）")
(princ)