;;; ============================================================
;;; EREC コマンド - 四角形・正方形作成
;;;
;;; 基点クリック後:
;;;   数値を入力 Enter → X幅として認識 → Y高さ入力 → 完成
;;;   S Enter         → 正方形モード（一辺の入力を1回で完了）
;;;   C Enter         → クリックモード（矩形プレビュー・スナップ有効）
;;; ============================================================

(defun C:EREC (/ pt1 pt2 width height size result)

  ;; ---- 基点 ----
  (setq pt1 (getpoint "\n基点を指定: "))
  (if (null pt1) (exit))
  (setq pt1 (list (car pt1) (cadr pt1) 0.0))

  ;; ---- X幅 / 正方形(S) / クリックモード(C) 選択 ----
  ;; initget でキーワード "S" と "C" を登録
  (initget "S C")
  (setq result (getreal "\nX方向(幅)を入力  または  [S=正方形 / C=クリックモード]: "))

  (cond

    ;; ---- S が入力された: 正方形モード ----
    ((= result "S")
     (initget 6) ; ゼロ・負数禁止
     (setq size (getreal "\n正方形の一辺の長さを入力: "))
     (if (null size) (exit))
     
     (setq pt2 (list (+ (car pt1) size) (+ (cadr pt1) size) 0.0))
     (command "_.RECTANG" pt1 pt2)
     (princ (strcat "\n作成完了(正方形): 一辺=" (rtos size 2 0) "mm"))
    )

    ;; ---- C が入力された: クリックモード ----
    ((= result "C")
     (setq pt2 (getcorner pt1 "\n対角点をクリック（スナップ有効）: "))
     (if (null pt2) (exit))
     (setq pt2 (list (car pt2) (cadr pt2) 0.0))
     (command "_.RECTANG" pt1 pt2)
     (princ
       (strcat "\n作成完了: 幅="
               (rtos (abs (- (car pt2)  (car pt1))) 2 0) "mm"
               "  高さ="
               (rtos (abs (- (cadr pt2) (cadr pt1))) 2 0) "mm"))
    )

    ;; ---- キャンセル ----
    ((null result) (exit))

    ;; ---- 数値入力: X幅確定 → Y高さ入力 ----
    (T
     (setq width result)
     (if (<= width 0) (progn (princ "\n無効な値です。") (exit)))

     (initget 6)
     (setq height (getreal
       (strcat "\nY方向(高さ)を入力 [幅=" (rtos width 2 0) "mm]: ")))
     (if (null height) (exit))

     (setq pt2 (list (+ (car pt1) width) (+ (cadr pt1) height) 0.0))
     (command "_.RECTANG" pt1 pt2)
     (princ (strcat "\n作成完了: 幅=" (rtos width 2 0) "mm  高さ=" (rtos height 2 0) "mm"))
    )

  )

  (princ)
)

(princ "\n[EREC] 四角形・正方形作成 - ロード完了")
(princ "\n  数値入力 : 基点 → X幅Enter → Y高さEnter → 完成")
(princ "\n  正方形   : 基点 → S Enter → 一辺の長さEnter → 完成")
(princ "\n  クリック : 基点 → C Enter → 対角点クリック → 完成")
(princ)