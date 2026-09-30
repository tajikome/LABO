;;; ================================================================
;;;  NUM.lsp  -  番号タグの連番付与 / CSV書き出し / ブロック属性
;;;
;;;   NUM  : 図形やブロックを順にクリックし、連番の番号タグを付ける
;;;          番号は英字付き(X1、Y1...)も可。開始時に英字だけ入力すると、
;;;          その英字の中で図面にある最大の番号の続きから始まる
;;;          同じ番号を複数の対象に付けるには、K(固定)かM(複数選択)を使う
;;;   NUMX : 番号ごとに、No・名称・幅・奥行・高さ・個数をCSVに書き出す
;;;          個数 = 同じ番号が付いた対象の数
;;;          サイズは、ブロックなら定義の外形×拡大率(回転は打ち消す)、
;;;          それ以外は軸に平行な外形。ブロック内に長さ *num-mark-len*(100)
;;;          の線分があれば、奥行きから *num-mark-sub*(50)を引く
;;;          属性(幅・奥行・高さ・品名)に入力があれば、測定値より優先する
;;;          エリア(=番号の英字部分)ごとに、CSVを分けて書き出す
;;;   NUMA : 既存のブロックに、標準の属性(階数・エリア・什器No・品名・色・幅・
;;;          奥行・高さ・数量・什器分類・備考)をまとめて追加し、図面に反映する
;;;          (ATTSYNC)。項目を変えたいときは *numa-tags* を書き換える。
;;;          配置位置: ブロックの最下部にある「中点」(POINT)があればそこ、
;;;          なければ最下辺の中央。そこから下方向へ200ピッチ、文字高さ125
;;;   NUMC : 1つのブロックの属性値を、他のブロックへコピーする(タグ名が
;;;          一致する項目だけ)。コピーしないタグは *numc-skip* で変更できる
;;;
;;;  ・番号タグ = 文字(既定)、または 丸+属性NO のブロック「NUM_TAG」
;;;    (NUMの対象選択中に T で切り替え)
;;;  ・文字タグは、既存の番号文字と同じプロパティ(画層・文字スタイルASA・
;;;    高さ400・幅係数0.7・位置合わせMC・ByLayer)で作成する
;;;  ・番号を付けた対象には、XDATA(アプリ名 NUM_APP)でタグのハンドルを記録
;;;  ・タグを図面から消すと、その番号は無効になる(NUMXにも出ない)
;;;  ・番号を直したいときは、タグを編集する(文字なら直接、丸付きブロック
;;;    なら属性値NOを編集)
;;;  ・実行時に表示される日本語の文字列は、\U+XXXX 形式のエスケープで
;;;    半角英数字として埋め込んであるため、ファイルの文字コードに関係なく
;;;    AutoCAD上では正しく日本語で表示される
;;;  ・コメント(この説明文を含む)は日本語のまま入っている。ファイルは
;;;    UTF-8(BOM付き)で保存してあるので、対応するエディタで開けば文字化け
;;;    しない。実行(AutoCADへの読み込み)には、コメントの表示は影響しない
;;; ================================================================
(vl-load-com)

(setq *num-app*    "NUM_APP"     ; XDATAのアプリケーション名
      *num-blk*    "NUM_TAG"     ; タグ用ブロックの名前(表示形式がBLOCKのときだけ使う)
      *num-lay*    "\U+30CA\U+30F3\U+30D0\U+30EA\U+30F3\U+30B0"  ; 番号タグの画層(すでにある画層は、そのまま使う)
      *num-tstyle* "ASA"         ; 文字(TEXT)タグの文字スタイル(なければ現在のスタイルを使う)
      *num-th*     400.0         ; 文字(TEXT)タグの文字高さ
      *num-mark-len* 100.0       ; ブロック内にこの長さの線分があれば、奥行き調整のマークとみなす
      *num-mark-sub* 50.0)       ; その線分がある場合、奥行きからこの値を引く

;;; ---- 補助関数 ------------------------------------------------

;; 図形の範囲(最小点 最大点)を返す。取得できなければ nil
(defun num:bbox (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

;; ブロック定義(元の図形)の外形(最小点 最大点)を、その定義自身の座標で返す。属性定義は含めない。
;; 結果はブロック名ごとに *num-bcache* にキャッシュする(NUMXを実行するたびにクリアする)。
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

;; 属性を配置する基準点を返す。ブロックの中に、外形の最下部(Y座標が外形の最小Yに近い)にある
;; ポイント(点)があれば、その点を使う。なければ、外形の最下辺の中点(X方向の中央・Yは最小値)
;; を使う。結果はブロック名ごとに *num-acache* にキャッシュする。
(defun num:blk-anchor (blkname / hit def e ext pt best)
  (if (setq hit (assoc blkname *num-acache*))
    (cdr hit)
    (progn
      (setq ext (num:blk-extents blkname)
            def (vl-catch-all-apply
                  'vla-Item
                  (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
                        blkname)))
      (if (and ext (not (vl-catch-all-error-p def)))
        (vlax-for e def
          (if (and (not best) (= (vla-get-ObjectName e) "AcDbPoint"))
            (progn
              (setq pt (vlax-safearray->list (vla-get-Coordinates e)))
              (if (< (abs (- (cadr pt) (cadr (car ext)))) 1.0)
                (setq best pt))))))
      (setq best (or best
                     (if ext
                       (list (/ (+ (car (car ext)) (car (cadr ext))) 2.0)
                             (cadr (car ext))
                             0.0))))
      (setq *num-acache* (cons (cons blkname best) *num-acache*))
      best
    )
  )
)

;; ブロック定義の中に、長さが *num-mark-len* の線分(LINE)があれば T を返す(ブロック名ごとにキャッシュ)
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

;; ブロックの回転を打ち消した (幅 奥行 高さ) を返す。
;; 値は、ブロック定義の外形 × 拡大率(絶対値)。測れない場合は nil。
;; 定義の中に長さ *num-mark-len* の線分があれば、奥行きから *num-mark-sub* を1回だけ引く
;; (その線分が何本あっても1回だけ引く)。
(defun num:block-size (ent / obj nm ext w d h)
  (setq obj (vlax-ename->vla-object ent))
  ;; vla-get-Name は実際の定義名(動的ブロックでは無名の *U.. になることがある)を返すため、動的ブロックにも対応できる
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

;; ブロックの属性値を ((タグ名 . 値) ...) の連想リストで返す。タグ名は大文字化する(属性がなければ nil)
(defun num:atts (obj / lst a)
  (if (= (vla-get-HasAttributes obj) :vlax-true)
    (foreach a (vlax-invoke obj 'GetAttributes)
      (setq lst (cons (cons (strcase (vla-get-TagString a)) (vla-get-TextString a)) lst)))
  )
  lst
)

;; 指定したタグの属性値(前後の空白を除去)を返す。項目がない、または空なら nil
(defun num:att-get (atts tag / v)
  (if (setq v (cdr (assoc (strcase tag) atts)))
    (progn
      (setq v (vl-string-trim " " v))
      (if (/= v "") v)
    )
  )
)

;; CSVの1項目分のテキストを返す(属性値がなければ空文字)。CSVを壊さないよう、
;; 中の二重引用符は単引用符に置き換える。
(defun num:csv-att (atts tag / v)
  (setq v (num:att-get atts tag))
  (if v (vl-string-translate "\"" "'" v) "")
)

;; 「1800」「 1800 」「1,800」「1800mm」 -> 1800.0 。それ以外は nil
(defun num:att-num (str / s i)
  (setq s (vl-string-trim " " str))
  ;; 桁区切りのカンマを取り除く
  (while (setq i (vl-string-search "," s))
    (setq s (strcat (if (> i 0) (substr s 1 i) "")
                    (if (< (1+ i) (strlen s)) (substr s (+ i 2)) ""))))
  (if (and (>= (strlen s) 2)
           (= (strcase (substr s (1- (strlen s)))) "MM"))
    (setq s (vl-string-trim " " (substr s 1 (- (strlen s) 2)))))
  (if (/= s "") (distof s 2))
)

;; サイズ1項目分の文字列を返す。指定したタグのどれかに値が入力されていれば、その値を使う
;; (数値は正規化し、「-」などの数値以外の文字はそのまま使う)。入力がなければ、測定した値
;; (測れなければ「-」)を使う。
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

;; 数値を四捨五入した整数の文字列にする(0.5以下は「-」)
(defun num:fmt (v)
  (if (> v 0.5) (itoa (fix (+ v 0.5))) "-")
)

;; 番号の文字列を (英字部分 数値 0埋め桁数 数字があったか) に分解する。
;;   「X1」->("X" 1 0 T)  「Y07」->("Y" 7 2 T)  「12」->("" 12 0 T)  「X」->("X" 1 0 nil)
;; 注記: AutoLISPの or/and は値ではなく T/nil を返すため、ここでは値の選択には使わない。
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

;; 現在の英字部分・番号・0埋め桁数から、表示する番号の文字列を作る
(defun num:label (n / s)
  (setq s (itoa n))
  (while (< (strlen s) *num-width*)
    (setq s (strcat "0" s))
  )
  (strcat *num-prefix* s)
)

;; 入力された番号の文字列(例: X1)から、現在の英字部分・番号・桁数を設定する
(defun num:set-label (str / p)
  (setq p (num:parse str))
  (setq *num-prefix* (car p)
        *num-width*  (caddr p)
        ;; 英字だけの入力(例: 「X」)の場合は、その英字の中で図面にある最大の番号の続きにする
        *num-next*   (if (cadddr p)
                       (cadr p)
                       (1+ (num:max-no (car p)))))
)

;; 図面にある(生きている)タグのうち、指定した英字部分で使われている最大の番号を返す(なければ0)
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

;; ファイル名に使えるよう、文字列中の使えない文字を置き換える
(defun num:safe (str)
  (vl-string-translate "\\/:*?\"<>|" "_________" str)
)

;; 対象に記録したタグのハンドルを返す(なければ nil)
(defun num:get-h (ent / xd)
  (setq xd (cdr (assoc -3 (entget ent (list *num-app*)))))
  (if xd (cdr (assoc 1005 (cdr (assoc *num-app* xd)))))
)

;; 対象にタグのハンドルを記録する(成功すれば非nil)
(defun num:put-h (ent h / r)
  (regapp *num-app*)
  (setq r (vl-catch-all-apply
            'entmod
            (list (append (entget ent)
                          (list (list -3 (list *num-app* (cons 1005 h))))))))
  (if (vl-catch-all-error-p r) nil r)
)

;; 対象の記録を消す
(defun num:clear-h (ent)
  (vl-catch-all-apply
    'entmod
    (list (list (cons -1 ent) (list -3 (list *num-app*)))))
)

;; ハンドルから、生きている番号タグの図形名を返す(なければ nil)
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

;; タグに表示されている番号を返す。文字(TEXT)ならその文字列、丸付きブロックなら属性NOの値
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

;; タグ用ブロック(円 + 属性NO)がなければ作る。使える状態なら T
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

;; タグ用の画層を用意する(なければ作成、ロック・非表示なら解除)
(defun num:prep-layer (doc / lay)
  (if (not (tblsearch "LAYER" *num-lay*))
    (vla-put-Color (vla-Add (vla-get-Layers doc) *num-lay*) 4)
  )
  (setq lay (vla-Item (vla-get-Layers doc) *num-lay*))
  (vla-put-Lock lay :vlax-false)
  (vla-put-LayerOn lay :vlax-true)
  (princ)
)

;; 点pt に、文字txtを表示する番号タグを作成する。
;; *num-style* が "TEXT"(文字、既定)か "BLOCK"(丸+属性)かで、作るものが変わる。
;; 作成したタグの図形名を返す。失敗した場合は nil。
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
               ;; 番号が長い場合は、丸の中に収まるよう文字を小さくする
               (if (> len 2)
                 (vl-catch-all-apply 'vla-put-Height
                                     (list a (/ (* 1.7 *num-r*) len)))))))
         (vlax-vla-object->ename blk)
       )
     ))
    (t
     ;; 文字(TEXT)の場合: 画層は *num-lay*、文字スタイルはASA、幅係数0.7、
     ;; 位置合わせはMC(中央中心)、色・線種・線の太さはByLayer
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

;;; ---- NUM : 番号タグを付ける -------------------------------------

;; 対象が番号タグ自身(タグの画層、またはタグ用ブロック)であれば T を返す
(defun num:tag-p (ed)
  (or (= (strcase (cdr (assoc 8 ed))) (strcase *num-lay*))
      (and (= (cdr (assoc 0 ed)) "INSERT")
           (= (strcase (cdr (assoc 2 ed))) *num-blk*)))
)

;; 対象entの範囲の中心に、文字txtのタグを付け、その記録を対象に残す。
;; oldh は、付け直しのときの以前のタグのハンドル(付け直しでなければ nil)。
;; (対象 タグ 以前のハンドル) を返す。失敗した場合は nil。
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
      ;; 何もない所をクリックした
      ((and (null sel) (= (getvar "ERRNO") 7))
       (princ "\n\U+5BFE\U+8C61\U+304C\U+9078\U+629E\U+3055\U+308C\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002"))
      ;; Enterキーで終了
      ((null sel) (setq done T))
      ;; オプションのキーワード
      ((= (type sel) 'STR)
       (cond
         ;; 直前に付けた番号を戻す(複数選択でまとめて付けた分も、まとめて1回として戻す)
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
         ;; 複数の対象をまとめて選び、すべてに同じ番号を付ける(タグは対象ごとに1つずつ作る)
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
         ;; 以降のクリックで、同じ番号を使い続ける(オンオフの切り替え)
         ((= sel "Keep")
          (if *num-hold*
            (progn
              (setq *num-hold* nil)
              ;; 現在の番号がすでに使われていれば、次の番号に進める
              (if (and stack
                       (= (nth 1 (car stack)) *num-prefix*)
                       (= (nth 2 (car stack)) *num-next*))
                (setq *num-next* (1+ *num-next*)))
              (princ (strcat "\n\U+56FA\U+5B9A\U+3092\U+89E3\U+9664\U+3057\U+307E\U+3057\U+305F\U+3002\U+6B21\U+306E\U+756A\U+53F7\U+306F " (num:label *num-next*) " \U+3067\U+3059\U+3002")))
            (progn
              (setq *num-hold* T)
              ;; 直前に使った番号を固定する
              (if stack
                (setq *num-prefix* (nth 1 (car stack))
                      *num-next*   (nth 2 (car stack))
                      *num-width*  (nth 3 (car stack))))
              (princ (strcat "\n\U+756A\U+53F7 " (num:label *num-next*)
                             " \U+3092\U+56FA\U+5B9A\U+3057\U+307E\U+3057\U+305F\U+3002\U+3082\U+3046\U+4E00\U+5EA6 K \U+3067\U+89E3\U+9664\U+3059\U+308B\U+3068\U+6B21\U+306E\U+756A\U+53F7\U+306B\U+9032\U+307F\U+307E\U+3059\U+3002")))))
       ))
      ;; 対象がクリックされた
      (t
       (setq ent (car sel) ed (entget ent) oldh nil)
       (cond
         ;; 番号タグ自身は対象から除く
         ((num:tag-p ed)
          (princ "\n\U+756A\U+53F7\U+30BF\U+30B0\U+81EA\U+4F53\U+306B\U+306F\U+4ED8\U+3051\U+3089\U+308C\U+307E\U+305B\U+3093\U+3002"))
         ;; すでに番号が付いている場合は、付け直すか確認する(いいえ の場合はそのまま)
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

;;; ---- NUMX : 番号付きの対象をCSVに書き出す -------------------------

(defun c:NUMX ( / ss i ent ed tag typ name bb sz val pr atts nm2 w d h nfloor narea nfix ncolor ncat nnote rows merged m mixed path f r areas ar file kinds total)

  (setq *num-bcache* nil *num-mcache* nil)
  ;; 番号が付いた対象ごとに、1行分のデータを集める
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
            ;; sz = (幅 奥行 高さ)。ブロックの場合は回転を打ち消した値(num:block-size を参照)。
            ;; それ以外、または測れない場合は、軸に平行な外形の値。
            (if (and (not sz) bb)
              (setq sz (list (- (car  (cadr bb)) (car  (car bb)))
                             (- (cadr (cadr bb)) (cadr (car bb)))
                             (- (caddr (cadr bb)) (caddr (car bb))))))
            ;; 属性(ブロックのみ): 値が入力されていれば、測定した値より優先する
            (setq atts (if (= typ "INSERT") (num:atts (vlax-ename->vla-object ent))))
            (if (setq nm2 (num:att-get atts "\U+54C1\U+540D")) (setq name (vl-string-translate "\"" "'" nm2)))
            (setq w (num:size-text atts '("\U+5E45") (if sz (car sz)))
                  d (num:size-text atts '("\U+5965\U+884C" "\U+5965\U+884C\U+304D") (if sz (cadr sz)))
                  h (num:size-text atts '("\U+9AD8\U+3055") (if sz (caddr sz))))
            ;; ブロックの、その他の属性(いずれも入力がなければ空文字)
            (setq nfloor (num:csv-att atts "\U+968E\U+6570")
                  narea  (num:csv-att atts "\U+30A8\U+30EA\U+30A2")
                  nfix   (num:csv-att atts "\U+4EC0\U+5668No")
                  ncolor (num:csv-att atts "\U+8272")
                  ncat   (num:csv-att atts "\U+4EC0\U+5668\U+5206\U+985E")
                  nnote  (num:csv-att atts "\U+5099\U+8003"))
            (if (not val) (setq val ""))
            (setq pr (num:parse val))
            ;; row = (英字部分 番号 表示番号 名称 幅 奥行 高さ 階数 エリア 什器No 色 什器分類 備考)
            (setq rows
                  (cons (list (strcase (car pr)) (cadr pr) val name
                              w d h nfloor narea nfix ncolor ncat nnote)
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
     ;; 同じ番号を持つ対象をまとめる。個数は、その番号を持つ対象の数。
     ;; サイズ・名称は、最初の1つの値を使う。まとめた1件 = (個数 英字部分 番号 表示番号 名称 幅 奥行 高さ)
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
     ;; エリア(=番号の英字部分)ごとに、CSVを1つずつ書き出す: <ファイル名>_<エリア>.csv
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
               (write-line "No,\U+540D\U+79F0,\U+5E45(mm),\U+5965\U+884C(mm),\U+9AD8\U+3055(mm),\U+500B\U+6570,\U+968E\U+6570,\U+30A8\U+30EA\U+30A2,\U+4EC0\U+5668No,\U+8272,\U+4EC0\U+5668\U+5206\U+985E,\U+5099\U+8003" f)
               (setq kinds 0 total 0)
               (foreach r rows
                 (if (= (cadr r) ar)
                   (progn
                     (write-line (strcat (cadddr r) ",\"" (nth 4 r) "\","
                                         (nth 5 r) "," (nth 6 r) "," (nth 7 r) ","
                                         (itoa (car r)) ",\""
                                         (nth 8 r) "\",\"" (nth 9 r) "\",\"" (nth 10 r)
                                         "\",\"" (nth 11 r) "\",\"" (nth 12 r) "\",\"" (nth 13 r) "\"")
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

;;; ---- NUMA : 既存のブロックに、標準の属性を追加する -----------------

;; 追加する属性のタグ一覧(項目を変えたいときは、この一覧を書き換える)。
;; それぞれ、非表示・プリセット(ブロックを置くときに聞かれない)属性になる。
(setq *numa-tags* '("\U+968E\U+6570" "\U+30A8\U+30EA\U+30A2" "\U+4EC0\U+5668No" "\U+54C1\U+540D" "\U+8272" "\U+5E45" "\U+5965\U+884C" "\U+9AD8\U+3055" "\U+6570\U+91CF" "\U+4EC0\U+5668\U+5206\U+985E" "\U+5099\U+8003"))

(defun c:NUMA ( / *error* doc oldecho ss i obj nm names def have e tag idx a added tot nblk anc)

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n\U+30A8\U+30E9\U+30FC: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *num-acache* nil)
  (princ "\n\U+5C5E\U+6027\U+3092\U+8FFD\U+52A0\U+3059\U+308B\U+30D6\U+30ED\U+30C3\U+30AF\U+3092\U+9078\U+629E\U+3057\U+3066\U+304F\U+3060\U+3055\U+3044")
  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      ;; 選んだ対象のブロック名を、重複なく集める(動的ブロックは元のブロック名)
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
            ;; このブロックに、すでに定義されている属性のタグ
            (setq have nil)
            (vlax-for e def
              (if (= (vla-get-ObjectName e) "AcDbAttributeDefinition")
                (setq have (cons (strcase (vla-get-TagString e)) have)))
            )
            (setq anc (num:blk-anchor nm))
            (setq added 0 idx 0)
            (foreach tag *numa-tags*
              (if (not (member (strcase tag) have))
                (progn
                  ;; 文字高さ125、モード9(非表示+プリセット)。基準点から下方向に、200ピッチで並べる。
                  ;; idx は *numa-tags* の中でのその項目の順番。あとから足りない項目だけをNUMAで
                  ;; 追加しても、各項目の行がずれたり重なったりしないようにするため。
                  (setq a (vl-catch-all-apply
                            'vla-AddAttribute
                            (list def 125.0 9 tag
                                  (vlax-3d-point (list (car anc) (- (cadr anc) (* 200.0 idx)) 0.0))
                                  tag "")))
                  (if (vl-catch-all-error-p a)
                    (princ (strcat "\n" nm ": \U+300C" tag "\U+300D\U+3092\U+8FFD\U+52A0\U+3067\U+304D\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F"))
                    (progn
                      (vla-put-Layer a "0")
                      (setq added (1+ added))))
                )
              )
              (setq idx (1+ idx))
            )
            (if (> added 0)
              (progn
                ;; 追加した属性定義を、図面にすでにあるそのブロックに反映する
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

;;; ---- NUMC : 属性の値を、他のブロックにコピーする -------------------

;; NUMCでコピーしないタグ(対象ごとに固有の値として残す。変えたいときはここを書き換える)。
(setq *numc-skip* '("\U+4EC0\U+5668No"))

(defun c:NUMC ( / *error* doc s0 sh sObj satts ss i tEnt tObj a tag v cnt objs skipped skiptags)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\n\U+30A8\U+30E9\U+30FC: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq s0 (entsel "\n\U+30B3\U+30D4\U+30FC\U+5143\U+306E\U+30D6\U+30ED\U+30C3\U+30AF(\U+5C5E\U+6027\U+304C\U+5165\U+3063\U+3066\U+3044\U+308B\U+3082\U+306E)\U+3092\U+9078\U+629E: "))
  (cond
    ((not s0)
     (princ "\n\U+5BFE\U+8C61\U+304C\U+9078\U+629E\U+3055\U+308C\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002"))
    ((/= (cdr (assoc 0 (entget (car s0)))) "INSERT")
     (princ "\n\U+30D6\U+30ED\U+30C3\U+30AF\U+3092\U+9078\U+629E\U+3057\U+3066\U+304F\U+3060\U+3055\U+3044\U+3002"))
    (t
     (setq sh   (cdr (assoc 5 (entget (car s0))))
           sObj (vlax-ename->vla-object (car s0))
           satts (num:atts sObj))
     (cond
       ((not satts)
        (princ "\n\U+3053\U+306E\U+30D6\U+30ED\U+30C3\U+30AF\U+306B\U+306F\U+5C5E\U+6027\U+304C\U+3042\U+308A\U+307E\U+305B\U+3093\U+3002"))
       (t
        (setq skiptags (mapcar 'strcase *numc-skip*))
        (princ "\n\U+30B3\U+30D4\U+30FC\U+5148\U+306E\U+30D6\U+30ED\U+30C3\U+30AF\U+3092\U+9078\U+629E(\U+8907\U+6570\U+53EF\U+30FB\U+7A93\U+9078\U+629E\U+3082\U+53EF)\U+3057\U+3066\U+304F\U+3060\U+3055\U+3044")
        (if (setq ss (ssget '((0 . "INSERT"))))
          (progn
            (setq i 0 cnt 0 objs 0 skipped 0)
            (repeat (sslength ss)
              (setq tEnt (ssname ss i) i (1+ i))
              (cond
                ;; コピー先の選択に、コピー元自身が含まれていた場合は除く
                ((= (cdr (assoc 5 (entget tEnt))) sh)
                 (setq skipped (1+ skipped)))
                (t
                 (setq tObj (vlax-ename->vla-object tEnt))
                 (if (= (vla-get-HasAttributes tObj) :vlax-true)
                   (progn
                     (setq objs (1+ objs))
                     (foreach a (vlax-invoke tObj 'GetAttributes)
                       (setq tag (strcase (vla-get-TagString a)))
                       (if (and (not (member tag skiptags))
                                (setq v (cdr (assoc tag satts))))
                         (progn
                           (vla-put-TextString a v)
                           (setq cnt (1+ cnt))))
                     )
                   )
                   (setq skipped (1+ skipped)))))
            )
            (princ (strcat "\n" (itoa objs) " \U+500B\U+306E\U+30D6\U+30ED\U+30C3\U+30AF\U+306B\U+3001\U+5408\U+8A08 " (itoa cnt) " \U+9805\U+76EE\U+3092\U+30B3\U+30D4\U+30FC\U+3057\U+307E\U+3057\U+305F\U+3002"
                           (if (> skipped 0)
                             (strcat "(\U+5BFE\U+8C61\U+5916 " (itoa skipped) " \U+500B)")
                             "")))
          )
          (princ "\n\U+30B3\U+30D4\U+30FC\U+5148\U+304C\U+9078\U+629E\U+3055\U+308C\U+307E\U+305B\U+3093\U+3067\U+3057\U+305F\U+3002")
        )
       )
     )
    )
  )
  (princ)
)

(princ "\nNUM(\U+756A\U+53F7\U+3092\U+4ED8\U+3051\U+308B) / NUMX(\U+500B\U+6570\U+3064\U+304DCSV\U+66F8\U+304D\U+51FA\U+3057) / NUMA(\U+30D6\U+30ED\U+30C3\U+30AF\U+306B\U+5C5E\U+6027\U+3092\U+8FFD\U+52A0) / NUMC(\U+5C5E\U+6027\U+306E\U+30B3\U+30D4\U+30FC) \U+3092\U+8AAD\U+307F\U+8FBC\U+307F\U+307E\U+3057\U+305F\U+3002")
(princ)