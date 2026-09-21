;;; ================================================================
;;;  NUM.lsp  -  numbered tags for objects / CSV export
;;;
;;;   NUM  : click objects or blocks one by one to place sequential number tags
;;;          labels may have a letter prefix (X1, Y1, ...); start with e.g. X1
;;;          typing letters only (e.g. X) continues after the highest existing number of that prefix
;;;   NUMX : export No, name, width, depth, height of numbered objects to CSV,
;;;          one file per area (= letter prefix): <name>_<area>.csv
;;;
;;;  - Number tag = plain TEXT (default) or circle block "NUM_TAG" (attribute NO);
;;;    switch with the T option of NUM
;;;  - TEXT tags copy the properties of the existing numbering text: layer, text style ASA,
;;;    height 400, width factor 0.7, justification Middle-Center (MC), ByLayer (see *num-lay* etc. below)
;;;  - Each numbered object stores the tag handle in XDATA (app name NUM_APP)
;;;  - Erasing a tag invalidates that number (NUMX skips it)
;;;  - To change a number, edit the tag's attribute (NO)
;;;  - This file is pure ASCII: Japanese messages are written as \U+ escapes
;;;    so it loads the same way regardless of the text encoding
;;; ================================================================
(vl-load-com)

(setq *num-app*    "NUM_APP"     ; XDATA application name
      *num-blk*    "NUM_TAG"     ; tag block name (only used by the BLOCK style)
      *num-lay*    "\U+30CA\U+30F3\U+30D0\U+30EA\U+30F3\U+30B0"  ; layer for number tags (existing layer is used as is)
      *num-tstyle* "ASA"         ; text style for TEXT tags (falls back to the current style)
      *num-th*     400.0)        ; text height of TEXT tags

;;; ---- Helper functions ------------------------------------------

;; Return (min-pt max-pt) of the object extents, or nil on failure
(defun num:bbox (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

;; Rounded integer string of a number (- when 0.5 or less)
(defun num:fmt (v)
  (if (> v 0.5) (itoa (fix (+ v 0.5))) "-")
)

;; Split a label into (prefix number zero-pad-width has-digits).
;;   "X1"->("X" 1 0 T)  "Y07"->("Y" 7 2 T)  "12"->("" 12 0 T)  "X"->("X" 1 0 nil)
;; NOTE: AutoLISP "or"/"and" return T/nil, not a value, so they are not used to pick values here.
(defun num:parse (s / len i d)
  (if (= s "")
    (list "" 0 0 nil)
    (progn
      (setq len (strlen s) i len)
      (while (and (> i 0) (wcmatch (substr s i 1) "#"))
        (setq i (1- i))
      )
      (setq d (if (< i len) (substr s (1+ i)) ""))
      (list (if (> i 0) (substr s 1 i) "")
            (if (= d "") 1 (atoi d))
            (if (and (> (strlen d) 1) (= (substr d 1 1) "0")) (strlen d) 0)
            (/= d ""))
    )
  )
)

;; Build the label text from the current prefix / number / zero-pad width
(defun num:label (n / s)
  (setq s (itoa n))
  (while (< (strlen s) *num-width*)
    (setq s (strcat "0" s))
  )
  (strcat *num-prefix* s)
)

;; Set the current prefix / number / width from a typed label such as X1
(defun num:set-label (str / p)
  (setq p (num:parse str))
  (setq *num-prefix* (car p)
        *num-width*  (caddr p)
        ;; letters only (e.g. "X"): continue after the highest existing number of that prefix
        *num-next*   (if (cadddr p)
                       (cadr p)
                       (1+ (num:max-no (car p)))))
)

;; Highest number in use for a prefix among the live tags in the drawing (0 if none)
(defun num:max-no (prefix / ss i ent tag val pr mx)
  (setq mx 0)
  (if (setq ss (ssget "_X" (list (list -3 (list *num-app*)))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (if (setq tag (num:tag-ename (num:get-h ent)))
          (if (setq val (num:tag-value tag))
            (progn
              (setq pr (num:parse val))
              (if (and (= (strcase (car pr)) (strcase prefix))
                       (> (cadr pr) mx))
                (setq mx (cadr pr)))))))))
  mx
)

;; Make a string safe for use in a file name
(defun num:safe (str)
  (vl-string-translate "\\/:*?\"<>|" "_________" str)
)

;; Return the tag handle stored on the object (nil if none)
(defun num:get-h (ent / xd)
  (setq xd (cdr (assoc -3 (entget ent (list *num-app*)))))
  (if xd (cdr (assoc 1005 (cdr (assoc *num-app* xd)))))
)

;; Store the tag handle on the object (non-nil on success)
(defun num:put-h (ent h / r)
  (regapp *num-app*)
  (setq r (vl-catch-all-apply
            'entmod
            (list (append (entget ent)
                          (list (list -3 (list *num-app* (cons 1005 h))))))))
  (if (vl-catch-all-error-p r) nil r)
)

;; Remove the stored record from the object
(defun num:clear-h (ent)
  (vl-catch-all-apply
    'entmod
    (list (list (cons -1 ent) (list -3 (list *num-app*)))))
)

;; Return the ename of the live number tag for a handle (nil if erased or missing)
(defun num:tag-ename (h / e ed)
  (if (and h
           (setq e (handent h))
           (not (vl-catch-all-error-p
                  (vl-catch-all-apply 'vlax-ename->vla-object (list e))))
           (not (vlax-erased-p (vlax-ename->vla-object e)))
           (setq ed (entget e))
           (or (and (= (cdr (assoc 0 ed)) "INSERT")
                    (= (strcase (cdr (assoc 2 ed))) *num-blk*))
               (member (cdr (assoc 0 ed)) '("TEXT" "MTEXT"))))
    e
  )
)

;; Return the displayed number of a tag: the TEXT string, or the NO attribute of a tag block
(defun num:tag-value (tag / ed e ed2 val)
  (setq ed (entget tag))
  (if (= (cdr (assoc 0 ed)) "INSERT")
    (progn
      (setq e (entnext tag))
      (while (and e (not val))
        (setq ed2 (entget e))
        (cond
          ((= (cdr (assoc 0 ed2)) "SEQEND") (setq e nil))
          ((and (= (cdr (assoc 0 ed2)) "ATTRIB")
                (= (strcase (cdr (assoc 2 ed2))) "NO"))
           (setq val (cdr (assoc 1 ed2))))
          (t (setq e (entnext e)))
        )
      )
      val
    )
    (cdr (assoc 1 ed))
  )
)

;; Create the tag block (circle + NO attribute) if missing. Returns T when usable
(defun num:make-block ()
  (if (not (tblsearch "BLOCK" *num-blk*))
    (progn
      (entmake (list '(0 . "BLOCK") (cons 2 *num-blk*) '(70 . 2) '(10 0.0 0.0 0.0)))
      (entmake '((0 . "CIRCLE") (8 . "0") (10 0.0 0.0 0.0) (40 . 1.0)))
      (entmake (list '(0 . "ATTDEF")
                     '(100 . "AcDbEntity")
                     '(8 . "0")
                     '(100 . "AcDbText")
                     '(10 0.0 0.0 0.0)
                     '(40 . 0.7)
                     '(1 . "1")
                     '(72 . 1)
                     '(11 0.0 0.0 0.0)
                     '(100 . "AcDbAttributeDefinition")
                     '(3 . "NO")
                     '(2 . "NO")
                     '(70 . 0)
                     '(74 . 2)))
      (entmake '((0 . "ENDBLK") (8 . "0")))
    )
  )
  (if (tblsearch "BLOCK" *num-blk*) T nil)
)

;; Prepare the tag layer (create if missing, unlock and turn on)
(defun num:prep-layer (doc / lay)
  (if (not (tblsearch "LAYER" *num-lay*))
    (vla-put-Color (vla-Add (vla-get-Layers doc) *num-lay*) 4)
  )
  (setq lay (vla-Item (vla-get-Layers doc) *num-lay*))
  (vla-put-Lock lay :vlax-false)
  (vla-put-LayerOn lay :vlax-true)
  (princ)
)

;; Create a number tag showing txt at point pt.
;; *num-style* = "TEXT" (plain text, default) or "BLOCK" (circle + attribute).
;; Returns the ename of the new tag, or nil on failure.
(defun num:make-tag (spc pt txt / blk a len)
  (cond
    ((= *num-style* "BLOCK")
     (if (num:make-block)
       (progn
         (setq blk (vla-InsertBlock spc (vlax-3d-point pt) *num-blk*
                                    *num-r* *num-r* *num-r* 0.0))
         (vla-put-Layer blk *num-lay*)
         (setq len (strlen txt))
         (foreach a (vlax-invoke blk 'GetAttributes)
           (if (= (strcase (vla-get-TagString a)) "NO")
             (progn
               (vla-put-TextString a txt)
               (vla-put-Layer a *num-lay*)
               ;; long labels: shrink the text so it stays inside the circle
               (if (> len 2)
                 (vl-catch-all-apply 'vla-put-Height
                                     (list a (/ (* 1.7 *num-r*) len)))))))
         (vlax-vla-object->ename blk)
       )
     ))
    (t
     ;; plain TEXT: layer *num-lay*, style ASA, width factor 0.7,
     ;; justification Middle-Center (MC), color/linetype/lineweight ByLayer
     (if (entmake (list '(0 . "TEXT")
                        (cons 8 *num-lay*)
                        (cons 10 pt)
                        (cons 40 *num-th*)
                        (cons 1 txt)
                        (cons 41 0.7)
                        (cons 7 (if (tblsearch "STYLE" *num-tstyle*)
                                  *num-tstyle*
                                  (getvar "TEXTSTYLE")))
                        '(72 . 1)
                        (cons 11 pt)
                        '(73 . 2)))
       (entlast)
     ))
  )
)

;;; ---- NUM : place number tags ---------------------------------

(defun c:NUM ( / *error* doc spc s0 s1 s2 txt done sel ent ed ok oldh bb pt tag h stack rec r)

  (defun *error* (msg)
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n\U+30A8\U+30E9\U+30FC: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        spc (vla-get-Block (vla-get-ActiveLayout doc)))
  (or *num-next* (setq *num-next* 1))
  (or *num-prefix* (setq *num-prefix* ""))
  (or *num-width* (setq *num-width* 0))
  (or *num-style* (setq *num-style* "TEXT"))
  (or *num-r* (setq *num-r* 150.0))
  (or *num-th* (setq *num-th* 400.0))

  (setq s0 (getstring (strcat "\n\U+958B\U+59CB\U+756A\U+53F7(\U+4F8B: X1 / \U+82F1\U+5B57\U+3060\U+3051\U+306A\U+3089\U+7D9A\U+304D\U+306E\U+756A\U+53F7) <" (num:label *num-next*) ">: ")))
  (if (and s0 (/= s0 "")) (num:set-label s0))

  (num:prep-layer doc)
  (vla-StartUndoMark doc)
  (setq done nil stack nil)
  (while (not done)
    (initget "Undo Size Number Type")
    (setvar "ERRNO" 0)
    (setq sel (entsel (strcat "\n\U+756A\U+53F7 " (num:label *num-next*)
                              " \U+3092\U+4ED8\U+3051\U+308B\U+5BFE\U+8C61\U+3092\U+9078\U+629E [\U+623B\U+3059(U)/\U+30B5\U+30A4\U+30BA(S)/\U+756A\U+53F7\U+5909\U+66F4(N)/\U+8868\U+793A\U+5F62\U+5F0F(T)] <\U+7D42\U+4E86>: ")))
    (cond
      ;; clicked on empty space
      ((and (null sel) (= (getvar "ERRNO") 7))
       (princ "\n\U+5BFE\U+8C61\U+304C\U+9078\U+629E\U+3055\U+308C\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002"))
      ;; Enter = finish
      ((null sel) (setq done T))
      ;; keyword options
      ((= (type sel) 'STR)
       (cond
         ((= sel "Undo")
          (if stack
            (progn
              (setq rec (car stack) stack (cdr stack))
              (entdel (cadr rec))
              (if (caddr rec)
                (num:put-h (car rec) (caddr rec))
                (num:clear-h (car rec)))
              (setq *num-prefix* (nth 3 rec) *num-next* (nth 4 rec) *num-width* (nth 5 rec))
              (princ "\n1\U+3064\U+623B\U+3057\U+307E\U+3057\U+305F\U+3002"))
            (princ "\n\U+623B\U+305B\U+308B\U+3082\U+306E\U+304C\U+3042\U+308A\U+307E\U+305B\U+3093\U+3002")))
         ((= sel "Size")
          (if (= *num-style* "BLOCK")
            (progn
              (setq r (getdist (strcat "\n\U+30BF\U+30B0\U+306E\U+534A\U+5F84 <" (rtos *num-r* 2 2) ">: ")))
              (if (and r (> r 0.0)) (setq *num-r* r)))
            (progn
              (setq r (getdist (strcat "\n\U+6587\U+5B57\U+306E\U+9AD8\U+3055 <" (rtos *num-th* 2 2) ">: ")))
              (if (and r (> r 0.0)) (setq *num-th* r)))))
         ((= sel "Number")
          (setq s1 (getstring (strcat "\n\U+756A\U+53F7(\U+4F8B: Y1 / \U+82F1\U+5B57\U+3060\U+3051\U+306A\U+3089\U+7D9A\U+304D\U+306E\U+756A\U+53F7) <" (num:label *num-next*) ">: ")))
          (if (and s1 (/= s1 "")) (num:set-label s1)))
         ((= sel "Type")
          (initget "Text Block")
          (setq s2 (getkword (strcat "\n\U+756A\U+53F7\U+306E\U+8868\U+793A\U+5F62\U+5F0F [\U+6587\U+5B57(T)/\U+4E38\U+4ED8\U+304D\U+30D6\U+30ED\U+30C3\U+30AF(B)] <"
                                     (if (= *num-style* "BLOCK") "\U+4E38\U+4ED8\U+304D\U+30D6\U+30ED\U+30C3\U+30AF" "\U+6587\U+5B57")
                                     ">: ")))
          (cond
            ((= s2 "Text") (setq *num-style* "TEXT"))
            ((= s2 "Block") (setq *num-style* "BLOCK"))))
       ))
      ;; an object was picked
      (t
       (setq ent (car sel) ed (entget ent) oldh nil ok T)
       (cond
         ;; number tags themselves (anything on the tag layer) are skipped
         ((or (= (strcase (cdr (assoc 8 ed))) (strcase *num-lay*))
              (and (= (cdr (assoc 0 ed)) "INSERT")
                   (= (strcase (cdr (assoc 2 ed))) *num-blk*)))
          (princ "\n\U+756A\U+53F7\U+30BF\U+30B0\U+81EA\U+4F53\U+306B\U+306F\U+4ED8\U+3051\U+3089\U+308C\U+307E\U+305B\U+3093\U+3002")
          (setq ok nil))
         ((num:tag-ename (num:get-h ent))
          (initget "Yes No")
          (if (= (getkword "\n\U+3059\U+3067\U+306B\U+756A\U+53F7\U+304C\U+4ED8\U+3044\U+3066\U+3044\U+307E\U+3059\U+3002\U+4ED8\U+3051\U+76F4\U+3057\U+307E\U+3059\U+304B? [\U+306F\U+3044(Y)/\U+3044\U+3044\U+3048(N)] <N>: ") "Yes")
            (setq oldh (num:get-h ent))
            (setq ok nil)))
       )
       (if ok
         (if (setq bb (num:bbox ent))
           (progn
             ;; place the tag at the center of the object extents
             (setq pt (list (/ (+ (car (car bb)) (car (cadr bb))) 2.0)
                            (/ (+ (cadr (car bb)) (cadr (cadr bb))) 2.0)
                            0.0))
             (setq txt (num:label *num-next*)
                   tag (num:make-tag spc pt txt))
             (if (and tag
                      (setq h (cdr (assoc 5 (entget tag))))
                      (num:put-h ent h))
               (progn
                 (setq stack (cons (list ent tag oldh *num-prefix* *num-next* *num-width*) stack))
                 (setq *num-next* (1+ *num-next*)))
               (progn
                 (if tag (entdel tag))
                 (princ "\n\U+3053\U+306E\U+5BFE\U+8C61\U+306B\U+306F\U+756A\U+53F7\U+3092\U+8A18\U+9332\U+3067\U+304D\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F(\U+753B\U+5C64\U+304C\U+30ED\U+30C3\U+30AF\U+3055\U+308C\U+3066\U+3044\U+307E\U+305B\U+3093\U+304B?)"))))
           (princ "\n\U+5BFE\U+8C61\U+306E\U+7BC4\U+56F2\U+3092\U+53D6\U+5F97\U+3067\U+304D\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002")))
      )
    )
  )
  (vla-EndUndoMark doc)
  (princ (strcat "\n\U+7D42\U+4E86\U+3057\U+307E\U+3057\U+305F\U+3002\U+6B21\U+306E\U+756A\U+53F7\U+306F " (num:label *num-next*) " \U+3067\U+3059\U+3002"))
  (princ)
)

;;; ---- NUMX : export numbered objects to CSV ---------------------

(defun c:NUMX ( / ss i ent ed tag typ name bb val pr rows path f prev dups r areas ar file cnt)

  (setq ss (ssget "_X" (list (list -3 (list *num-app*)))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (if (setq tag (num:tag-ename (num:get-h ent)))
          (progn
            (setq ed   (entget ent)
                  typ  (cdr (assoc 0 ed))
                  name (if (= typ "INSERT")
                         (vla-get-EffectiveName (vlax-ename->vla-object ent))
                         typ)
                  bb   (num:bbox ent)
                  val  (num:tag-value tag))
            (if (not val) (setq val ""))
            (setq pr (num:parse val))
            ;; row = (PREFIX NUMBER LABEL NAME WIDTH DEPTH HEIGHT)
            (setq rows
                  (cons (list (strcase (car pr)) (cadr pr) val name
                              (if bb (num:fmt (- (car  (cadr bb)) (car  (car bb)))) "-")
                              (if bb (num:fmt (- (cadr (cadr bb)) (cadr (car bb)))) "-")
                              (if bb (num:fmt (- (caddr (cadr bb)) (caddr (car bb)))) "-"))
                        rows))
          )
        )
      )
    )
  )

  (cond
    ((null rows)
     (princ "\n\U+756A\U+53F7\U+3092\U+4ED8\U+3051\U+305F\U+5BFE\U+8C61\U+304C\U+898B\U+3064\U+304B\U+308A\U+307E\U+305B\U+3093\U+3002"))
    (t
     (setq rows (vl-sort rows
                         '(lambda (a b)
                            (or (< (car a) (car b))
                                (and (= (car a) (car b)) (< (cadr a) (cadr b)))))))
     ;; Check for duplicate numbers
     (setq prev nil dups nil)
     (foreach r rows
       (if (and prev (= (car r) (car prev)) (= (cadr r) (cadr prev)))
         (setq dups (cons (caddr r) dups)))
       (setq prev r)
     )
     (if dups
       (princ (strcat "\n\U+6CE8\U+610F: \U+91CD\U+8907\U+3057\U+3066\U+3044\U+308B\U+756A\U+53F7\U+304C\U+3042\U+308A\U+307E\U+3059 \U+2192"
                      (apply 'strcat
                             (mapcar '(lambda (x) (strcat " " x))
                                     (reverse dups))))))
     ;; one CSV per area (= letter prefix): <name>_<area>.csv
     (setq path (getfiled "\U+4FDD\U+5B58\U+5148\U+3068\U+30D5\U+30A1\U+30A4\U+30EB\U+540D(\U+30A8\U+30EA\U+30A2\U+3054\U+3068\U+306B _\U+30A8\U+30EA\U+30A2\U+540D \U+304C\U+4ED8\U+3044\U+3066\U+4FDD\U+5B58\U+3055\U+308C\U+307E\U+3059)"
                          (strcat (getvar "DWGPREFIX")
                                  (vl-filename-base (getvar "DWGNAME"))
                                  "_\U+756A\U+53F7.csv")
                          "csv" 1))
     (if path
       (progn
         (setq areas nil)
         (foreach r rows
           (if (not (member (car r) areas))
             (setq areas (append areas (list (car r))))))
         (foreach ar areas
           (setq file (strcat (vl-filename-directory path) "/"
                              (vl-filename-base path) "_"
                              (num:safe (if (= ar "") "\U+306A\U+3057" ar)) ".csv"))
           (if (setq f (open file "w"))
             (progn
               (write-line "No,\U+540D\U+79F0,\U+5E45(mm),\U+5965\U+884C(mm),\U+9AD8\U+3055(mm)" f)
               (setq cnt 0)
               (foreach r rows
                 (if (= (car r) ar)
                   (progn
                     (write-line (strcat (caddr r) ",\"" (cadddr r) "\","
                                         (nth 4 r) "," (nth 5 r) "," (nth 6 r))
                                 f)
                     (setq cnt (1+ cnt)))))
               (close f)
               (princ (strcat "\n" (if (= ar "") "\U+306A\U+3057" ar) ": " (itoa cnt) " \U+4EF6 \U+2192 " file)))
             (princ (strcat "\n\U+30D5\U+30A1\U+30A4\U+30EB\U+3092\U+958B\U+3051\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F(Excel\U+3067\U+958B\U+3044\U+3066\U+3044\U+307E\U+305B\U+3093\U+304B?): " file))))))
    )
  )
  (princ)
)

(princ "\nNUM(\U+756A\U+53F7\U+3092\U+4ED8\U+3051\U+308B) / NUMX(CSV\U+66F8\U+304D\U+51FA\U+3057) \U+3092\U+8AAD\U+307F\U+8FBC\U+307F\U+307E\U+3057\U+305F\U+3002")
(princ)