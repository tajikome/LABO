(defun c:KL ( / pt p1 p2 old_osmode)
  (setq pt (getpoint "\n中心となる点を選択: "))
  (if pt
    (progn
      (setq p1 (list (car pt) (- (cadr pt) 50) (caddr pt))
            p2 (list (car pt) (+ (cadr pt) 50) (caddr pt)))
      
      ;; オブジェクトスナップを一時的に無効化
      (setq old_osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0)
      
      (command "_.LINE" p1 p2 "")
      
      ;; スナップ設定を元に戻す
      (setvar "OSMODE" old_osmode)
    )
  )
  (princ)
)