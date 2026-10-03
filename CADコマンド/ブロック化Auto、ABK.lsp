;;; ABK.lsp - 選択した図形を「近接するまとまり」ごとに自動でブロック化
;;; 基点 = 各まとまりのバウンディングボックス左下
;;; 使い方: APPLOAD で読み込み → ABK → 図形を窓選択 → 結合距離を入力

(vl-load-com)

;; 図形のバウンディングボックス (minx miny maxx maxy) / 取得不可なら nil
(defun abk-bbox (en / obj mn mx r)
  (setq obj (vlax-ename->vla-object en))
  (setq r (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
  (if (vl-catch-all-error-p r)
    nil
    (progn
      (setq mn (vlax-safearray->list mn)
            mx (vlax-safearray->list mx))
      (list (car mn) (cadr mn) (car mx) (cadr mx))
    )
  )
)

;; グループ = (minx miny maxx maxy 図形リスト)
(defun abk-overlap (a b tol)
  (and (<= (- (car a) tol) (nth 2 b))
       (>= (+ (nth 2 a) tol) (car b))
       (<= (- (cadr a) tol) (nth 3 b))
       (>= (+ (nth 3 a) tol) (cadr b))
  )
)

(defun abk-merge (a b)
  (list (min (car a) (car b))
        (min (cadr a) (cadr b))
        (max (nth 2 a) (nth 2 b))
        (max (nth 3 a) (nth 3 b))
        (append (nth 4 a) (nth 4 b))
  )
)

;; 未使用のブロック名を返す
(defun abk-newname (/ n nm)
  (setq n 1)
  (while (tblsearch "BLOCK" (setq nm (strcat "BLK_" (itoa n))))
    (setq n (1+ n))
  )
  nm
)

(defun c:ABK (/ ss tol i en bb g groups rest changed cnt os ce nm ss2 pt)
  (setq ss (ssget))
  (if ss
    (progn
      (setq tol (getdist "\n同じブロックとみなす最大の隙間 <5>: "))
      (if (null tol) (setq tol 5.0))

      ;; --- グルーピング ---
      (setq i 0 groups nil)
      (repeat (sslength ss)
        (setq en (ssname ss i)
              i  (1+ i)
              bb (abk-bbox en)
        )
        (if bb
          (progn
            (setq g (append bb (list (list en))))
            (setq changed T)
            (while changed
              (setq changed nil rest nil)
              (foreach h groups
                (if (abk-overlap g h tol)
                  (setq g (abk-merge g h) changed T)
                  (setq rest (cons h rest))
                )
              )
              (setq groups rest)
            )
            (setq groups (cons g groups))
          )
        )
      )

      ;; --- ブロック化 ---
      (setq os (getvar "OSMODE") ce (getvar "CMDECHO"))
      (setvar "OSMODE" 0)
      (setvar "CMDECHO" 0)
      (command "_.undo" "_begin")
      (setq cnt 0)
      (foreach g groups
        (setq nm  (abk-newname)
              pt  (list (car g) (cadr g) 0.0)   ; 左下
              ss2 (ssadd)
        )
        (foreach e (nth 4 g) (ssadd e ss2))
        (command "_.-block" nm pt ss2 "")
        (entmake
          (list '(0 . "INSERT")
                (cons 2 nm)
                (cons 8 (getvar "CLAYER"))
                (cons 10 pt)
                '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                '(50 . 0.0)
          )
        )
        (setq cnt (1+ cnt))
      )
      (command "_.undo" "_end")
      (setvar "OSMODE" os)
      (setvar "CMDECHO" ce)
      (princ (strcat "\n" (itoa cnt) " 個のブロックを作成しました。"))
    )
  )
  (princ)
)

(princ "\nABK を読み込みました。")
(princ)