;;; IJCAD / AutoCAD compatible - ASCII source
;;; ABC: bounding-box center
;;; ABM: bounding-box bottom center
;;; IJCAD / AutoCAD 互換版
;;; ABC: 図形外接範囲の中心をブロック基点にします。
;;; ABM: 図形外接範囲の最下部中点をブロック基点にします。
;;; 注記: 挿入済みブロックの見た目位置と属性位置は維持します。

(setq ABM_Mode nil)

(defun c:ABC (/ mode doc blks ss i en bn names data bp blk mn mx e
                    lo hi sx sy sz ang v w refs j ref a)
  (vl-load-com)
  (setq mode (if ABM_Mode 'BOTTOM-MID 'CENTER)
        doc (vla-get-activedocument (vlax-get-acad-object))
        blks (vla-get-blocks doc))
  (prompt (if (= mode 'CENTER)
            "\nSelect blocks for center base point: "
            "\nSelect blocks for bottom-center base point: "))
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (setq i 0 names '())
      (repeat (sslength ss)
        (setq bn (cdr (assoc 2 (entget (ssname ss i)))))
        (if (and (/= (substr bn 1 1) "*") (not (member bn names)))
          (setq names (cons bn names)))
        (setq i (1+ i)))
      ;; 選択されたブロック定義を一つずつ処理します。
      (foreach bn names
        (setq blk (vla-item blks bn) lo nil hi nil)
        ;; 属性定義を除く図形の外接範囲を求めます。
        (vlax-for e blk
          (if (/= (vla-get-objectname e) "AcDbAttributeDefinition")
            (if (not (vl-catch-all-error-p
                       (vl-catch-all-apply 'vla-getboundingbox (list e 'mn 'mx))))
              (progn
                (setq mn (vlax-safearray->list mn)
                      mx (vlax-safearray->list mx))
                (setq lo (if lo (mapcar 'min lo mn) mn)
                      hi (if hi (mapcar 'max hi mx) mx))))))
        (if (and lo hi)
          (progn
            ;; ABC は外接範囲の中心、ABM は最下部の中点を使います。
            (setq bp (if (= mode 'CENTER)
                       (mapcar '(lambda (x y) (/ (+ x y) 2.0)) lo hi)
                       (list (/ (+ (car lo) (car hi)) 2.0) (cadr lo) 0.0)))
            ;; 定義内の図形を移動し、新しい基点を原点にします。
            (vlax-for e blk
              (vla-move e (vlax-3d-point bp) (vlax-3d-point '(0 0 0))))
            ;; 挿入済みブロックを補正して表示位置を維持します。
            (if (setq refs (ssget "_X" (list '(0 . "INSERT") (cons 2 bn))))
              (progn
                (setq j 0)
                (repeat (sslength refs)
                  (setq en (ssname refs j)
                        data (entget en)
                        sx (cdr (assoc 41 data))
                        sy (cdr (assoc 42 data))
                        sz (cdr (assoc 43 data))
                        ang (cdr (assoc 50 data))
                        v (list (- (* sx (car bp) (cos ang)) (* sy (cadr bp) (sin ang)))
                                (+ (* sx (car bp) (sin ang)) (* sy (cadr bp) (cos ang)))
                                (* sz (caddr bp)))
                        w (trans v en 0 T)
                        ref (vlax-ename->vla-object en))
                  (vla-move ref (vlax-3d-point '(0 0 0)) (vlax-3d-point w))
                  (if (= (vla-get-hasattributes ref) :vlax-true)
                    (foreach a (vlax-invoke ref 'getattributes)
                      (vla-move a (vlax-3d-point w) (vlax-3d-point '(0 0 0)))))
                  (setq j (1+ j))))))
          (prompt (strcat "\n" bn " : skipped (no geometry)"))))
      (vla-regen doc acactiveviewport)
      (prompt (strcat "\nUpdated " (itoa (length names)) " block definition(s).")))
    (prompt "\nNo blocks selected."))
  (princ))

(defun c:ABM ()
  (setq ABM_Mode T)
  (c:ABC)
  (setq ABM_Mode nil)
  (princ))

(princ "\nABC: Center / ABM: Bottom center")
(princ)