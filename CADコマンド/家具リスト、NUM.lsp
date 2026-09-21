;;; ================================================================
;;;  NUM.lsp  -  numbered tags for objects / CSV export
;;;
;;;   NUM  : click objects or blocks one by one to place sequential number tags
;;;          labels may have a letter prefix (X1, Y1, ...); start with e.g. X1
;;;          typing letters only (e.g. X) continues after the highest existing number of that prefix
;;;          same number on several objects = quantity: K (keep the number) or M (multi-select)
;;;   NUMX : export No, name, width, depth, height, quantity to CSV,
;;;          quantity = how many objects carry the same number;
;;;          block sizes = block definition extents x |scale| (rotation cancelled); other objects = axis-aligned extents;
;;;          a LINE of length *num-mark-len* (100) inside a block definition => depth reduced by *num-mark-sub* (50);
;;;          block attributes win when filled in: name <- attribute for the product name, width/depth/height <- attributes
;;;          (empty attribute => measured value);
;;;          one file per area (= letter prefix): <name>_<area>.csv
;;;
;;;   NUMA : add attribute definitions (floor, area, fixture No, name, height, quantity, category,
;;;          notes) to existing blocks and synchronize them (ATTSYNC); edit *numa-tags* to change the items
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
      *num-th*     400.0         ; text height of TEXT tags
      *num-mark-len* 100.0       ; a LINE of this length inside a block = depth marker
      *num-mark-sub* 50.0)       ; depth is reduced by this much when such a LINE exists

;;; ---- Helper functions ------------------------------------------

;; Return (min-pt max-pt) of the object extents, or nil on failure
(defun num:bbox (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

;; Extents (min-pt max-pt) of a block DEFINITION in its own coordinates, attribute definitions excluded.
;; Results are cached per block name in *num-bcache* (cleared at the start of each NUMX run).
(defun num:blk-extents (blkname / hit def e mn mx lo hi ext)
  (if (setq hit (assoc blkname *num-bcache*))
    (cdr hit)
    (progn
      (setq def (vl-catch-all-apply
                  'vla-Item
                  (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
                        blkname)))
      (if (not (vl-catch-all-error-p def))
        (vlax-for e def
          (if (/= (vla-get-ObjectName e) "AcDbAttributeDefinition")
            (if (not (vl-catch-all-error-p
                       (vl-catch-all-apply 'vla-GetBoundingBox (list e 'mn 'mx))))
              (progn
                (setq lo (vlax-safearray->list mn)
                      hi (vlax-safearray->list mx))
                (setq ext (if ext
                            (list (mapcar 'min (car ext) lo)
                                  (mapcar 'max (cadr ext) hi))
                            (list lo hi))))))))
      (setq *num-bcache* (cons (cons blkname ext) *num-bcache*))
      ext
    )
  )
)

;; T when the block definition contains a LINE whose length is *num-mark-len* (cached per block name)
(defun num:blk-marker (blkname / hit def e found)
  (if (setq hit (assoc blkname *num-mcache*))
    (cdr hit)
    (progn
      (setq def (vl-catch-all-apply
                  'vla-Item
                  (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
                        blkname)))
      (if (not (vl-catch-all-error-p def))
        (vlax-for e def
          (if (and (not found)
                   (= (vla-get-ObjectName e) "AcDbLine")
                   (< (abs (- (vla-get-Length e) *num-mark-len*)) 0.01))
            (setq found T))
        )
      )
      (setq *num-mcache* (cons (cons blkname found) *num-mcache*))
      found
    )
  )
)

;; (width depth height) of a block reference with the rotation cancelled:
;; extents of the definition x |scale factors|.  nil if it cannot be measured.
;; If the definition contains a LINE of length *num-mark-len*, the depth is reduced
;; by *num-mark-sub* (once, however many such lines there are).
(defun num:block-size (ent / obj nm ext w d h)
  (setq obj (vlax-ename->vla-object ent))
  ;; vla-get-Name = the actual (possibly anonymous *U..) definition, so dynamic blocks are handled
  (setq nm (vla-get-Name obj))
  (if (setq ext (num:blk-extents nm))
    (progn
      (setq w (* (abs (vla-get-XScaleFactor obj)) (- (car  (cadr ext)) (car  (car ext))))
            d (* (abs (vla-get-YScaleFactor obj)) (- (cadr (cadr ext)) (cadr (car ext))))
            h (* (abs (vla-get-ZScaleFactor obj)) (- (caddr (cadr ext)) (caddr (car ext)))))
      (if (num:blk-marker nm)
        (setq d (- d *num-mark-sub*)))
      (list w d h)
    )
  )
)

;; Attribute values of a block reference as an alist ((TAG . text) ...), tags upper-cased (nil if none)
(defun num:atts (obj / lst a)
  (if (= (vla-get-HasAttributes obj) :vlax-true)
    (foreach a (vlax-invoke obj 'GetAttributes)
      (setq lst (cons (cons (strcase (vla-get-TagString a)) (vla-get-TextString a)) lst)))
  )
  lst
)

;; Trimmed text of the attribute with this tag, or nil when it is missing or empty
(defun num:att-get (atts tag / v)
  (if (setq v (cdr (assoc (strcase tag) atts)))
    (progn
      (setq v (vl-string-trim " " v))
      (if (/= v "") v)
    )
  )
)

;; "1800" / " 1800 " / "1,800" / "1800mm" -> 1800.0 ; anything else -> nil
(defun num:att-num (str / s i)
  (setq s (vl-string-trim " " str))
  ;; remove thousands separators
  (while (setq i (vl-string-search "," s))
    (setq s (strcat (if (> i 0) (substr s 1 i) "")
                    (if (< (1+ i) (strlen s)) (substr s (+ i 2)) ""))))
  (if (and (>= (strlen s) 2)
           (= (strcase (substr s (1- (strlen s)))) "MM"))
    (setq s (vl-string-trim " " (substr s 1 (- (strlen s) 2)))))
  (if (/= s "") (distof s 2))
)

;; Text for one size column.  If any of the attribute tags has a value, that value is used
;; (numbers are normalized, other text such as "-" is kept as typed); otherwise the measured
;; value (or "-" when nothing could be measured).
(defun num:size-text (atts tags measured / v n tg)
  (foreach tg tags
    (if (and (not v) (setq n (num:att-get atts tg)))
      (setq v n)))
  (cond
    (v (if (setq n (num:att-num v))
         (num:fmt n)
         (vl-string-translate ",\"" "  " v)))
    (measured (num:fmt measured))
    (t "-")
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

;; True when the entity is a number tag itself (tag layer, or the tag block)
(defun num:tag-p (ed)
  (or (= (strcase (cdr (assoc 8 ed))) (strcase *num-lay*))
      (and (= (cdr (assoc 0 ed)) "INSERT")
           (= (strcase (cdr (assoc 2 ed))) *num-blk*)))
)

;; Put a tag with the text txt on object ent (center of its extents) and record it on the object.
;; oldh = handle of the previous tag when re-numbering (or nil).
;; Returns (ent tag oldh), or nil on failure.
(defun num:tag-object (ent spc txt oldh / bb pt tag h)
  (cond
    ((not (setq bb (num:bbox ent)))
     (princ "\n\U+5BFE\U+8C61\U+306E\U+7BC4\U+56F2\U+3092\U+53D6\U+5F97\U+3067\U+304D\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002")
     nil)
    (t
     (setq pt (list (/ (+ (car (car bb)) (car (cadr bb))) 2.0)
                    (/ (+ (cadr (car bb)) (cadr (cadr bb))) 2.0)
                    0.0))
     (setq tag (num:make-tag spc pt txt))
     (cond
       ((and tag
             (setq h (cdr (assoc 5 (entget tag))))
             (num:put-h ent h))
        (list ent tag oldh))
       (t
        (if tag (entdel tag))
        (princ "\n\U+3053\U+306E\U+5BFE\U+8C61\U+306B\U+306F\U+756A\U+53F7\U+3092\U+8A18\U+9332\U+3067\U+304D\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F(\U+753B\U+5C64\U+304C\U+30ED\U+30C3\U+30AF\U+3055\U+308C\U+3066\U+3044\U+307E\U+305B\U+3093\U+304B?)")
        nil))
    )
  )
)

(defun c:NUM ( / *error* doc spc s0 s1 s2 done sel ent ed oldh item items ss i skipped r stack rec)

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
  (setq *num-hold* nil)

  (setq s0 (getstring (strcat "\n\U+958B\U+59CB\U+756A\U+53F7(\U+4F8B: X1 / \U+82F1\U+5B57\U+3060\U+3051\U+306A\U+3089\U+7D9A\U+304D\U+306E\U+756A\U+53F7) <" (num:label *num-next*) ">: ")))
  (if (and s0 (/= s0 "")) (num:set-label s0))

  (num:prep-layer doc)
  (vla-StartUndoMark doc)
  (setq done nil stack nil)
  (while (not done)
    (initget "Undo Size Number Type Multi Keep")
    (setvar "ERRNO" 0)
    (setq sel (entsel (strcat "\n\U+756A\U+53F7 " (num:label *num-next*)
                              (if *num-hold* "(\U+56FA\U+5B9A\U+4E2D)" "")
                              " \U+3092\U+4ED8\U+3051\U+308B\U+5BFE\U+8C61\U+3092\U+9078\U+629E [\U+623B\U+3059(U)/\U+8907\U+6570\U+9078\U+629E(M)/\U+540C\U+3058\U+756A\U+53F7\U+3092\U+7D9A\U+3051\U+308B(K)/\U+756A\U+53F7\U+5909\U+66F4(N)/\U+30B5\U+30A4\U+30BA(S)/\U+8868\U+793A\U+5F62\U+5F0F(T)] <\U+7D42\U+4E86>: ")))
    (cond
      ;; clicked on empty space
      ((and (null sel) (= (getvar "ERRNO") 7))
       (princ "\n\U+5BFE\U+8C61\U+304C\U+9078\U+629E\U+3055\U+308C\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002"))
      ;; Enter = finish
      ((null sel) (setq done T))
      ;; keyword options
      ((= (type sel) 'STR)
       (cond
         ;; undo the last placement (a whole multi-selection counts as one)
         ((= sel "Undo")
          (if stack
            (progn
              (setq rec (car stack) stack (cdr stack))
              (foreach item (car rec)
                (entdel (cadr item))
                (if (caddr item)
                  (num:put-h (car item) (caddr item))
                  (num:clear-h (car item))))
              (setq *num-prefix* (nth 1 rec) *num-next* (nth 2 rec) *num-width* (nth 3 rec))
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
         ;; several objects at once, all get the same number (each object gets its own tag)
         ((= sel "Multi")
          (princ "\n\U+540C\U+3058\U+756A\U+53F7\U+3092\U+4ED8\U+3051\U+308B\U+5BFE\U+8C61\U+3092\U+9078\U+629E\U+3057\U+3066\U+304F\U+3060\U+3055\U+3044(\U+7A93\U+9078\U+629E\U+30FB\U+4EA4\U+5DEE\U+9078\U+629E\U+306A\U+3069)")
          (if (setq ss (ssget))
            (progn
              (setq items nil skipped 0 i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i) i (1+ i))
                (if (or (num:tag-p (entget ent))
                        (num:tag-ename (num:get-h ent))
                        (not (setq item (num:tag-object ent spc (num:label *num-next*) nil))))
                  (setq skipped (1+ skipped))
                  (setq items (cons item items))))
              (if items
                (progn
                  (setq stack (cons (list items *num-prefix* *num-next* *num-width*) stack))
                  (princ (strcat "\n" (num:label *num-next*) " \U+3092 " (itoa (length items)) " \U+500B\U+306B\U+4ED8\U+3051\U+307E\U+3057\U+305F"
                                 (if (> skipped 0)
                                   (strcat "(\U+30B9\U+30AD\U+30C3\U+30D7 " (itoa skipped) " \U+500B)")
                                   "")))
                  (if (not *num-hold*) (setq *num-next* (1+ *num-next*))))
                (princ "\n\U+756A\U+53F7\U+3092\U+4ED8\U+3051\U+3089\U+308C\U+308B\U+5BFE\U+8C61\U+304C\U+3042\U+308A\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002")))))
         ;; keep using the same number for the following picks (toggle)
         ((= sel "Keep")
          (if *num-hold*
            (progn
              (setq *num-hold* nil)
              ;; if the current label has been used, advance to the next number
              (if (and stack
                       (= (nth 1 (car stack)) *num-prefix*)
                       (= (nth 2 (car stack)) *num-next*))
                (setq *num-next* (1+ *num-next*)))
              (princ (strcat "\n\U+56FA\U+5B9A\U+3092\U+89E3\U+9664\U+3057\U+307E\U+3057\U+305F\U+3002\U+6B21\U+306E\U+756A\U+53F7\U+306F " (num:label *num-next*) " \U+3067\U+3059\U+3002")))
            (progn
              (setq *num-hold* T)
              ;; hold the label used last
              (if stack
                (setq *num-prefix* (nth 1 (car stack))
                      *num-next*   (nth 2 (car stack))
                      *num-width*  (nth 3 (car stack))))
              (princ (strcat "\n\U+756A\U+53F7 " (num:label *num-next*)
                             " \U+3092\U+56FA\U+5B9A\U+3057\U+307E\U+3057\U+305F\U+3002\U+3082\U+3046\U+4E00\U+5EA6 K \U+3067\U+89E3\U+9664\U+3059\U+308B\U+3068\U+6B21\U+306E\U+756A\U+53F7\U+306B\U+9032\U+307F\U+307E\U+3059\U+3002")))))
       ))
      ;; an object was picked
      (t
       (setq ent (car sel) ed (entget ent) oldh nil)
       (cond
         ;; number tags themselves are skipped
         ((num:tag-p ed)
          (princ "\n\U+756A\U+53F7\U+30BF\U+30B0\U+81EA\U+4F53\U+306B\U+306F\U+4ED8\U+3051\U+3089\U+308C\U+307E\U+305B\U+3093\U+3002"))
         ;; already numbered: ask whether to re-number (N = leave as is)
         ((and (num:tag-ename (num:get-h ent))
               (progn
                 (initget "Yes No")
                 (/= (getkword "\n\U+3059\U+3067\U+306B\U+756A\U+53F7\U+304C\U+4ED8\U+3044\U+3066\U+3044\U+307E\U+3059\U+3002\U+4ED8\U+3051\U+76F4\U+3057\U+307E\U+3059\U+304B? [\U+306F\U+3044(Y)/\U+3044\U+3044\U+3048(N)] <N>: ") "Yes")))
          nil)
         (t
          (if (num:tag-ename (num:get-h ent)) (setq oldh (num:get-h ent)))
          (if (setq item (num:tag-object ent spc (num:label *num-next*) oldh))
            (progn
              (setq stack (cons (list (list item) *num-prefix* *num-next* *num-width*) stack))
              (if (not *num-hold*) (setq *num-next* (1+ *num-next*)))))))
      )
    )
  )
  (vla-EndUndoMark doc)
  (princ (strcat "\n\U+7D42\U+4E86\U+3057\U+307E\U+3057\U+305F\U+3002\U+6B21\U+306E\U+756A\U+53F7\U+306F " (num:label *num-next*) " \U+3067\U+3059\U+3002"))
  (princ)
)

;;; ---- NUMX : export numbered objects to CSV ---------------------

(defun c:NUMX ( / ss i ent ed tag typ name bb sz val pr atts nm2 w d h rows merged m mixed path f r areas ar file kinds total)

  (setq *num-bcache* nil *num-mcache* nil)
  ;; collect one row per numbered object
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
                  sz   (if (= typ "INSERT") (num:block-size ent))
                  val  (num:tag-value tag))
            ;; sz = (width depth height).  Blocks: rotation cancelled (see num:block-size).
            ;; Anything else (or if that fails): axis-aligned extents.
            (if (and (not sz) bb)
              (setq sz (list (- (car  (cadr bb)) (car  (car bb)))
                             (- (cadr (cadr bb)) (cadr (car bb)))
                             (- (caddr (cadr bb)) (caddr (car bb))))))
            ;; attributes (blocks only): a filled-in value wins over the measured one
            (setq atts (if (= typ "INSERT") (num:atts (vlax-ename->vla-object ent))))
            (if (setq nm2 (num:att-get atts "\U+54C1\U+540D")) (setq name (vl-string-translate "\"" "'" nm2)))
            (setq w (num:size-text atts '("\U+5E45") (if sz (car sz)))
                  d (num:size-text atts '("\U+5965\U+884C" "\U+5965\U+884C\U+304D") (if sz (cadr sz)))
                  h (num:size-text atts '("\U+9AD8\U+3055") (if sz (caddr sz))))
            (if (not val) (setq val ""))
            (setq pr (num:parse val))
            ;; row = (PREFIX NUMBER LABEL NAME WIDTH DEPTH HEIGHT)
            (setq rows
                  (cons (list (strcase (car pr)) (cadr pr) val name
                              w d h)
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
     ;; merge objects that share the same number: quantity = how many objects carry it.
     ;; size / name of the first object are used.  merged item = (COUNT PREFIX NUMBER LABEL NAME W D H)
     (setq merged nil mixed nil)
     (foreach r rows
       (setq m (car merged))
       (cond
         ((and m (= (car r) (cadr m)) (= (cadr r) (caddr m)))
          (if (and (/= (cadddr r) (nth 4 m))
                   (not (member (cadddr m) mixed)))
            (setq mixed (cons (cadddr m) mixed)))
          (setq merged (cons (cons (1+ (car m)) (cdr m)) (cdr merged))))
         (t
          (setq merged (cons (cons 1 r) merged))))
     )
     (setq rows (reverse merged))
     (if mixed
       (princ (strcat "\n\U+6CE8\U+610F: \U+540C\U+3058\U+756A\U+53F7\U+306A\U+306E\U+306B\U+540D\U+79F0(\U+30D6\U+30ED\U+30C3\U+30AF\U+540D\U+30FB\U+56F3\U+5F62\U+306E\U+7A2E\U+985E)\U+304C\U+9055\U+3046\U+5BFE\U+8C61\U+304C\U+3042\U+308A\U+307E\U+3059 \U+2192"
                      (apply 'strcat
                             (mapcar '(lambda (x) (strcat " " x))
                                     (reverse mixed))))))
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
           (if (not (member (cadr r) areas))
             (setq areas (append areas (list (cadr r))))))
         (foreach ar areas
           (setq file (strcat (vl-filename-directory path) "/"
                              (vl-filename-base path) "_"
                              (num:safe (if (= ar "") "\U+306A\U+3057" ar)) ".csv"))
           (if (setq f (open file "w"))
             (progn
               (write-line "No,\U+540D\U+79F0,\U+5E45(mm),\U+5965\U+884C(mm),\U+9AD8\U+3055(mm),\U+500B\U+6570" f)
               (setq kinds 0 total 0)
               (foreach r rows
                 (if (= (cadr r) ar)
                   (progn
                     (write-line (strcat (cadddr r) ",\"" (nth 4 r) "\","
                                         (nth 5 r) "," (nth 6 r) "," (nth 7 r) ","
                                         (itoa (car r)))
                                 f)
                     (setq kinds (1+ kinds) total (+ total (car r))))))
               (close f)
               (princ (strcat "\n" (if (= ar "") "\U+306A\U+3057" ar) ": "
                              (itoa kinds) " \U+7A2E\U+985E / \U+500B\U+6570 " (itoa total) " \U+2192 " file)))
             (princ (strcat "\n\U+30D5\U+30A1\U+30A4\U+30EB\U+3092\U+958B\U+3051\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F(Excel\U+3067\U+958B\U+3044\U+3066\U+3044\U+307E\U+305B\U+3093\U+304B?): " file))))))
    )
  )
  (princ)
)

;;; ---- NUMA : add the standard attribute definitions to existing blocks ----

;; Attribute tags to add (edit this list to change the items).
;; Each becomes an invisible, preset attribute (not asked when the block is inserted).
(setq *numa-tags* '("\U+968E\U+6570" "\U+30A8\U+30EA\U+30A2" "\U+4EC0\U+5668No" "\U+54C1\U+540D" "\U+5E45" "\U+5965\U+884C" "\U+9AD8\U+3055" "\U+6570\U+91CF" "\U+4EC0\U+5668\U+5206\U+985E" "\U+5099\U+8003"))

(defun c:NUMA ( / *error* doc oldecho ss i obj nm names def have e tag k a added tot nblk)

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n\U+30A8\U+30E9\U+30FC: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n\U+5C5E\U+6027\U+3092\U+8FFD\U+52A0\U+3059\U+308B\U+30D6\U+30ED\U+30C3\U+30AF\U+3092\U+9078\U+629E\U+3057\U+3066\U+304F\U+3060\U+3055\U+3044")
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      ;; unique block names (dynamic blocks: the base block name)
      (setq i 0 names nil)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i)) i (1+ i))
        (setq nm (vla-get-EffectiveName obj))
        (if (and (/= (strcase nm) *num-blk*)
                 (not (member nm names)))
          (setq names (cons nm names)))
      )
      (setq names (reverse names) tot 0 nblk 0)
      (setq oldecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (vla-StartUndoMark doc)
      (foreach nm names
        (setq def (vla-Item (vla-get-Blocks doc) nm))
        (if (= (vla-get-IsXRef def) :vlax-true)
          (princ (strcat "\n" nm ": \U+5916\U+90E8\U+53C2\U+7167\U+306E\U+305F\U+3081\U+30B9\U+30AD\U+30C3\U+30D7\U+3057\U+307E\U+3057\U+305F"))
          (progn
            ;; tags already defined in this block
            (setq have nil)
            (vlax-for e def
              (if (= (vla-get-ObjectName e) "AcDbAttributeDefinition")
                (setq have (cons (strcase (vla-get-TagString e)) have)))
            )
            (setq added 0 k 0)
            (foreach tag *numa-tags*
              (if (not (member (strcase tag) have))
                (progn
                  ;; height 2.5, mode 9 = invisible + preset; stacked below the base point
                  (setq a (vl-catch-all-apply
                            'vla-AddAttribute
                            (list def 2.5 9 tag
                                  (vlax-3d-point (list 0.0 (* -4.0 k) 0.0))
                                  tag "")))
                  (if (vl-catch-all-error-p a)
                    (princ (strcat "\n" nm ": \U+300C" tag "\U+300D\U+3092\U+8FFD\U+52A0\U+3067\U+304D\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F"))
                    (progn
                      (vla-put-Layer a "0")
                      (setq added (1+ added) k (1+ k))))
                )
              )
            )
            (if (> added 0)
              (progn
                ;; push the new definitions to the block references already in the drawing
                (command "_.ATTSYNC" "_N" nm)
                (setq tot (+ tot added) nblk (1+ nblk))
                (princ (strcat "\n" nm ": " (itoa added) " \U+9805\U+76EE\U+3092\U+8FFD\U+52A0\U+3057\U+3066\U+53CD\U+6620\U+3057\U+307E\U+3057\U+305F")))
              (princ (strcat "\n" nm ": \U+3059\U+3079\U+3066\U+8FFD\U+52A0\U+6E08\U+307F\U+3067\U+3059")))
          )
        )
      )
      (vla-EndUndoMark doc)
      (setvar "CMDECHO" oldecho)
      (princ (strcat "\n\U+5B8C\U+4E86: " (itoa nblk) " \U+7A2E\U+985E\U+306E\U+30D6\U+30ED\U+30C3\U+30AF\U+306B\U+3001\U+5408\U+8A08 " (itoa tot) " \U+9805\U+76EE\U+3092\U+8FFD\U+52A0\U+3057\U+307E\U+3057\U+305F\U+3002"))
    )
    (princ "\n\U+30D6\U+30ED\U+30C3\U+30AF\U+304C\U+9078\U+629E\U+3055\U+308C\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002")
  )
  (princ)
)

(princ "\nNUM(\U+756A\U+53F7\U+3092\U+4ED8\U+3051\U+308B) / NUMX(\U+500B\U+6570\U+3064\U+304DCSV\U+66F8\U+304D\U+51FA\U+3057) / NUMA(\U+30D6\U+30ED\U+30C3\U+30AF\U+306B\U+5C5E\U+6027\U+3092\U+8FFD\U+52A0) \U+3092\U+8AAD\U+307F\U+8FBC\U+307F\U+307E\U+3057\U+305F\U+3002")
(princ)