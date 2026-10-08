;;; ================================================================
;;;  NUM.lsp  -  番号タグの連番付与 / CSV書き出し / ブロック属性
;;;
;;;   NUM  : 図形やブロックを順にクリックし、連番の番号タグを付ける
;;;          番号は英字付き(X1、Y1...)も可。開始時に英字だけ入力すると、
;;;          その英字の中で図面にある最大の番号の続きから始まる
;;;          同じ番号を複数の対象に付けるには、K(固定)かM(複数選択)を使う
;;;          対象がブロックで、属性「什器No」を持っていれば、その値を番号に自動で合わせる
;;;          コマンドを開始する前に、番号(タグ、または番号を付けた対象)を1個だけ
;;;          選んでいれば、その続きの番号から始める(基本は、そのまま連番で進める)
;;;          すでに番号が付いている対象を選ぶと、付け直すか確認する(既定はYES)
;;;   NUMX : 番号ごとに、什器No・名称・幅・奥行・高さ・個数・階数・エリア・
;;;          什器分類・色・備考をCSVに書き出す
;;;          番号タグがないブロックでも、属性「什器No」に値が入っていれば対象に含める
;;;          (番号タグがあれば、タグの番号を優先する)
;;;          同じ番号でも、サイズ(幅・奥行・高さ)が違うものは別の行に分けて書き出す
;;;          個数 = 各対象の「個数」属性の合計(未入力は1としてカウント)
;;;          サイズは、ブロックなら定義の外形×拡大率(回転は打ち消す)、
;;;          それ以外は軸に平行な外形。ブロック内に長さ *num-mark-len*(100)
;;;          の線分があれば、奥行きから *num-mark-sub*(50)を引く
;;;          ブロック内にTRI150の三角形(一辺150)があれば、サイズ計算から除外する
;;;          属性(幅・奥行・高さ・品名)に入力があれば、測定値より優先する
;;;          出力時に、分けない/エリア(属性)ごと/階数(属性)ごと、のいずれかを確認する
;;;          (番号の英字部分では分けない)
;;;   NUMA : 既存のブロックの属性を、標準の項目(什器No・階数・エリア・品名・幅・
;;;          奥行・高さ・個数・什器分類・色・備考)で作り直す(既存の属性はいった
;;;          ん削除してから作成。NUMCのnum:replace-tagsと同じ方式)。図面に反映
;;;          する(ATTSYNC)。項目を変えたいときは *numa-tags* を書き換える。
;;;          配置位置: ブロックの最下部にある「中点」(POINT)があればそこ、
;;;          なければ最下辺の中央。そこから下方向へ200ピッチ、文字高さ125
;;;   NUMC : 1つのブロックの属性値を、他のブロックへコピーする(全項目、什器Noを除く)。
;;;          コピー元・コピー先とも、コマンドを開始する前に選んでいれば、その選択を
;;;          使える(1個目がコピー元、2個目以降があればコピー先になる)。コピー先の
;;;          属性(コピー元とは無関係なものも含む)はいったんすべて削除し、コピー元
;;;          と同じ項目を作り直してから、値をコピーする。既存の値は失われる
;;;   NUMF : 図面にある、属性を持つブロックから「タグ: 値」の組み合わせを件数付きで
;;;          集め、PowerShell(WPF)のダイアログで検索したいものを選ぶ(複数可)。
;;;          見つかった対象は、図面上で選択状態になる(続けてNUMCやNUMIに使える)
;;;   NUMI : NUMCと同じく、1つのブロックの属性値を他のブロックへコピーするが、
;;;          コピーする項目を、PowerShell(WPF)のダイアログで選べる(複数可、既定は
;;;          未選択)。コピー先にすでにある項目は値を置き換え、関係のない(選ばな
;;;          かった)属性には触れない
;;;
;;;  ・番号タグ = 文字(既定)、または 丸+属性NO のブロック「NUM_TAG」
;;;    (NUMの対象選択中に T で切り替え)
;;;  ・文字タグは、既存の番号文字と同じプロパティ(画層・文字スタイルMeiryo UI・
;;;    高さ250・幅係数0.7・位置合わせMC・ByLayer)で作成する
;;;  ・番号を付けた対象には、XDATA(アプリ名 NUM_APP)でタグのハンドルを記録
;;;  ・タグを図面から消すと、その番号は無効になる(NUMXにも出ない。ただし、
;;;    属性「什器No」に値が入っているブロックは、その値でNUMXに出る)
;;;  ・番号を直したいときは、タグを編集する(文字なら直接、丸付きブロック
;;;    なら属性値NOを編集)
;;;  ・NUMFとNUMIのダイアログは、Windows + PowerShellが必要(QSコマンドと同じ、
;;;    埋め込んだPowerShellスクリプトを一時ファイルに書き出しWScript.Shell経由で
;;;    実行する方式)。AutoCAD上では未検証。白基調のチェックリストで、
;;;    「タグ: 値 (N件)」はタグ名(項目)ごとのカードに分け、項目のカードを左右2列に並べる。値のどこを
;;;    クリックしてもチェックでき、絞り込み・すべて選択/解除・選択件数の表示がある
;;;  ・このファイルは、日本語(全角)の文字をそのまま含んでいる(文字列・
;;;    コメントとも)。文字コードは UTF-8(BOM付き)で保存する
;;;  ・AutoCAD 2021以降は、システム変数 LISPSYS の既定値(1)でAutoLISPが
;;;    Unicodeに完全対応しており、この形式でそのまま正しく読み込める。
;;;    LISPSYSが0(2020以前の互換モード)の場合は、Unicodeに対応しないため、
;;;    文字コードをシフトJIS(ANSI)に変換したファイルが別途必要になる
;;; ================================================================
(vl-load-com)

(setq *num-app*    "NUM_APP"     ; XDATAのアプリケーション名
      *num-blk*    "NUM_TAG"     ; タグ用ブロックの名前(表示形式がBLOCKのときだけ使う)
      *num-lay*    "ナンバリング"  ; 番号タグの画層(すでにある画層は、そのまま使う)
      *num-tstyle* "Meiryo UI"   ; 文字(TEXT)タグの文字スタイル(なければ現在のスタイルを使う)
      *num-th*     250.0         ; 文字(TEXT)タグの文字高さ
      *num-mark-len* 100.0       ; ブロック内にこの長さの線分があれば、奥行き調整のマークとみなす
      *num-mark-sub* 50.0        ; その線分がある場合、奥行きからこの値を引く
      *num-tri-len*  150.0)      ; 一辺がこの長さの三角形(TRI150)は、サイズ計算から除外する

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
          ;; 属性定義と、TRI150の三角形マーカーは、サイズ計算から除外する
          (if (and (/= (vla-get-ObjectName e) "AcDbAttributeDefinition")
                   (not (num:tri-p e)))
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
      ;; NOTE: AutoLISPの or は値ではなく T/nil を返すため、ここでは or で値を選ばない
      (if (not best)
        (setq best (if ext
                     (list (/ (+ (car (car ext)) (car (cadr ext))) 2.0)
                           (cadr (car ext))
                           0.0))))
      (setq *num-acache* (cons (cons blkname best) *num-acache*))
      best
    )
  )
)

;; e が、一辺 *num-tri-len* の閉じた三角形(頂点3つ・弧なしのLWPOLYLINE)なら T を返す。
;; TRI150コマンドで描いた逆三角形を見分けるためのもので、サイズ計算から除外する対象になる。
(defun num:tri-p (e / ed a pts n i p1 p2 bulge ok)
  (setq ed (entget (vlax-vla-object->ename e)))
  (cond
    ((/= (cdr (assoc 0 ed)) "LWPOLYLINE") nil)
    ((= (logand (cdr (assoc 70 ed)) 1) 0) nil) ; 閉じていないポリラインは対象外
    (t
     (setq pts nil bulge nil)
     (foreach a ed
       (cond
         ((= (car a) 10) (setq pts (cons (cdr a) pts)))
         ((and (= (car a) 42) (> (abs (cdr a)) 0.0001)) (setq bulge T))
       )
     )
     (setq pts (reverse pts) n (length pts))
     (cond
       (bulge nil)
       ((/= n 3) nil)
       (t
        (setq ok T i 0)
        (repeat n
          (setq p1 (nth i pts) p2 (nth (rem (1+ i) n) pts))
          (if (> (abs (- (distance p1 p2) *num-tri-len*)) 0.5) (setq ok nil))
          (setq i (1+ i))
        )
        ok
       )
     )
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

;; 「個数」属性の値を数値として返す。入力がない・数値として読めない場合は 1
(defun num:att-qty (atts / v n)
  (setq v (num:att-get atts "個数"))
  (if (and v (setq n (num:att-num v))) n 1.0)
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

;; ブロックの属性「什器No」があれば、その値を番号の表示文字txtに合わせる
(defun num:sync-fixno (ent txt / obj a)
  (if (and (= (cdr (assoc 0 (entget ent))) "INSERT")
           (= (vla-get-HasAttributes (setq obj (vlax-ename->vla-object ent))) :vlax-true))
    (foreach a (vlax-invoke obj 'GetAttributes)
      (if (= (strcase (vla-get-TagString a)) "什器NO")
        (vla-put-TextString a txt)))
  )
  (princ) ; 戻り値は使わない
)

;; 対象ent(タグ自身、または番号を付けた対象)から、表示されている番号を読み取る。
;; 読み取れなければ nil。NUMの開始時、事前選択から続きの番号を決めるのに使う。
(defun num:pre-number (ent / h tag)
  (cond
    ((num:tag-p (entget ent)) (num:tag-value ent))
    ((and (setq h (num:get-h ent)) (setq tag (num:tag-ename h))) (num:tag-value tag))
    (t nil)
  )
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
     (princ "\n対象の範囲を取得できませんでした。")
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
        (num:sync-fixno ent txt)
        (list ent tag oldh))
       (t
        (if tag (entdel tag))
        (princ "\nこの対象には番号を記録できませんでした(画層がロックされていませんか?)")
        nil))
    )
  )
)

(defun c:NUM ( / *error* doc spc s0 s1 s2 done sel ent ed oldh item items ss i skipped r stack rec)

  (defun *error* (msg)
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nエラー: " msg))
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
  (or *num-th* (setq *num-th* 250.0))
  (setq *num-hold* nil)

  (setq s0 (getstring (strcat "\n開始番号(例: X1 / 英字だけなら続きの番号) <" (num:label *num-next*) ">: ")))
  (if (and s0 (/= s0 "")) (num:set-label s0))

  (num:prep-layer doc)
  (vla-StartUndoMark doc)
  (setq done nil stack nil)
  (while (not done)
    (initget "Undo Size Number Type Multi Keep")
    (setvar "ERRNO" 0)
    (setq sel (entsel (strcat "\n番号 " (num:label *num-next*)
                              (if *num-hold* "(固定中)" "")
                              " を付ける対象を選択 [戻す(U)/複数選択(M)/同じ番号を続ける(K)/番号変更(N)/サイズ(S)/表示形式(T)] <終了>: ")))
    (cond
      ;; 何もない所をクリックした
      ((and (null sel) (= (getvar "ERRNO") 7))
       (princ "\n対象が選択されませんでした。"))
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
              (princ "\n1つ戻しました。"))
            (princ "\n戻せるものがありません。")))
         ((= sel "Size")
          (if (= *num-style* "BLOCK")
            (progn
              (setq r (getdist (strcat "\nタグの半径 <" (rtos *num-r* 2 2) ">: ")))
              (if (and r (> r 0.0)) (setq *num-r* r)))
            (progn
              (setq r (getdist (strcat "\n文字の高さ <" (rtos *num-th* 2 2) ">: ")))
              (if (and r (> r 0.0)) (setq *num-th* r)))))
         ((= sel "Number")
          (setq s1 (getstring (strcat "\n番号(例: Y1 / 英字だけなら続きの番号) <" (num:label *num-next*) ">: ")))
          (if (and s1 (/= s1 "")) (num:set-label s1)))
         ((= sel "Type")
          (initget "Text Block")
          (setq s2 (getkword (strcat "\n番号の表示形式 [文字(T)/丸付きブロック(B)] <"
                                     (if (= *num-style* "BLOCK") "丸付きブロック" "文字")
                                     ">: ")))
          (cond
            ((= s2 "Text") (setq *num-style* "TEXT"))
            ((= s2 "Block") (setq *num-style* "BLOCK"))))
         ;; 複数の対象をまとめて選び、すべてに同じ番号を付ける(タグは対象ごとに1つずつ作る)
         ((= sel "Multi")
          (princ "\n同じ番号を付ける対象を選択してください(窓選択・交差選択など)")
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
                  (princ (strcat "\n" (num:label *num-next*) " を " (itoa (length items)) " 個に付けました"
                                 (if (> skipped 0)
                                   (strcat "(スキップ " (itoa skipped) " 個)")
                                   "")))
                  (if (not *num-hold*) (setq *num-next* (1+ *num-next*))))
                (princ "\n番号を付けられる対象がありませんでした。")))))
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
              (princ (strcat "\n固定を解除しました。次の番号は " (num:label *num-next*) " です。")))
            (progn
              (setq *num-hold* T)
              ;; 直前に使った番号を固定する
              (if stack
                (setq *num-prefix* (nth 1 (car stack))
                      *num-next*   (nth 2 (car stack))
                      *num-width*  (nth 3 (car stack))))
              (princ (strcat "\n番号 " (num:label *num-next*)
                             " を固定しました。もう一度 K で解除すると次の番号に進みます。")))))
       ))
      ;; 対象がクリックされた
      (t
       (setq ent (car sel) ed (entget ent) oldh nil)
       (cond
         ;; 番号タグ自身は対象から除く
         ((num:tag-p ed)
          (princ "\n番号タグ自体には付けられません。"))
         ;; すでに番号が付いている場合は、付け直すか確認する(いいえ の場合はそのまま)
         ((and (num:tag-ename (num:get-h ent))
               (progn
                 (initget "Yes No")
                 (setq s1 (getkword "\nすでに番号が付いています。付け直しますか? [はい(Y)/いいえ(N)] <Y>: "))
                 (= s1 "No")))
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
  (princ (strcat "\n終了しました。次の番号は " (num:label *num-next*) " です。"))
  (princ)
)

;;; ---- NUMX : 番号付きの対象をCSVに書き出す -------------------------

;; 対象entの、CSV1行分のデータを返す。val は表示番号(什器No)。
;; row = (英字部分 番号 表示番号(什器No) 名称 幅 奥行 高さ 個数 階数 エリア 色 備考 什器分類)
(defun num:x-row (ent val / ed typ name bb sz atts nm2 w d h pr)
  (setq ed   (entget ent)
        typ  (cdr (assoc 0 ed))
        name (if (= typ "INSERT")
               (vla-get-EffectiveName (vlax-ename->vla-object ent))
               typ)
        bb   (num:bbox ent)
        sz   (if (= typ "INSERT") (num:block-size ent)))
  ;; sz = (幅 奥行 高さ)。ブロックの場合は回転を打ち消した値(num:block-size を参照)。
  ;; それ以外、または測れない場合は、軸に平行な外形の値。
  (if (and (not sz) bb)
    (setq sz (list (- (car  (cadr bb)) (car  (car bb)))
                   (- (cadr (cadr bb)) (cadr (car bb)))
                   (- (caddr (cadr bb)) (caddr (car bb))))))
  ;; 属性(ブロックのみ): 値が入力されていれば、測定した値より優先する
  (setq atts (if (= typ "INSERT") (num:atts (vlax-ename->vla-object ent))))
  (if (setq nm2 (num:att-get atts "品名")) (setq name (vl-string-translate "\"" "'" nm2)))
  (setq w (num:size-text atts '("幅") (if sz (car sz)))
        d (num:size-text atts '("奥行" "奥行き") (if sz (cadr sz)))
        h (num:size-text atts '("高さ") (if sz (caddr sz))))
  (if (not val) (setq val ""))
  (setq pr (num:parse val))
  ;; 個数(属性)は、入力がなければ 1 としてカウントする。
  ;; その他の属性は、いずれも入力がなければ空文字
  (list (strcase (car pr)) (cadr pr) val name
        w d h (num:att-qty atts)
        (num:csv-att atts "階数")
        (num:csv-att atts "エリア")
        (num:csv-att atts "色")
        (num:csv-att atts "備考")
        (num:csv-att atts "什器分類"))
)

;; サイズの文字列 a が b より前に並ぶなら T(数値どうしは数値で比べ、数値は「-」などより前)
(defun num:size-lt (a b / x y)
  (setq x (distof a 2) y (distof b 2))
  (cond
    ((and x y) (< x y))
    (x T)
    (y nil)
    (t (< a b))
  )
)

;; CSVの行の並び順: 英字部分 → 番号 → 幅 → 奥行 → 高さ
(defun num:row-lt (a b)
  (cond
    ((/= (car a) (car b)) (< (car a) (car b)))
    ((/= (cadr a) (cadr b)) (< (cadr a) (cadr b)))
    ((/= (nth 4 a) (nth 4 b)) (num:size-lt (nth 4 a) (nth 4 b)))
    ((/= (nth 5 a) (nth 5 b)) (num:size-lt (nth 5 a) (nth 5 b)))
    (t (num:size-lt (nth 6 a) (nth 6 b)))
  )
)

(defun c:NUMX ( / ss i ent tag val rows merged m mixed sizediff s3 split path groups gl grp f r file kinds total)

  (setq *num-bcache* nil *num-mcache* nil)
  ;; 番号タグが付いた対象ごとに、1行分のデータを集める(表示番号はタグの番号)
  (if (setq ss (ssget "_X" (list (list -3 (list *num-app*)))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (if (setq tag (num:tag-ename (num:get-h ent)))
          (setq rows (cons (num:x-row ent (num:tag-value tag)) rows))))))
  ;; 番号タグがないブロックでも、属性「什器No」に値が入っていれば対象にする(表示番号はその値)
  (if (setq ss (ssget "_X" '((0 . "INSERT") (66 . 1))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (if (and (not (num:tag-ename (num:get-h ent)))
                 (not (num:tag-p (entget ent)))
                 (setq val (num:att-get (num:atts (vlax-ename->vla-object ent)) "什器No")))
          (setq rows (cons (num:x-row ent val) rows))))))

  (cond
    ((null rows)
     (princ "\n番号を付けた対象(または什器Noが入力されたブロック)が見つかりません。"))
    (t
     (setq rows (vl-sort rows 'num:row-lt))
     ;; 同じ番号で、サイズ(幅・奥行・高さ)も同じ対象をまとめる。サイズが違うものは別の行にする。
     ;; 個数は、各対象の「個数」属性の合計(未入力は1)。名称などは、最初の1つの値を使う。
     ;; まとめた1件 = (個数 英字部分 番号 表示番号(什器No) 名称 幅 奥行 高さ ...)
     (setq merged nil mixed nil sizediff nil)
     (foreach r rows
       (setq m (car merged))
       (cond
         ((and m (= (car r) (cadr m)) (= (cadr r) (caddr m))
               (= (nth 4 r) (nth 5 m)) (= (nth 5 r) (nth 6 m)) (= (nth 6 r) (nth 7 m)))
          (if (and (/= (cadddr r) (nth 4 m))
                   (not (member (cadddr m) mixed)))
            (setq mixed (cons (cadddr m) mixed)))
          (setq merged (cons (cons (+ (car m) (nth 7 r)) (cdr m)) (cdr merged))))
         (t
          ;; 同じ番号でサイズが違う(並び順で直前が同じ番号)
          (if (and m (= (car r) (cadr m)) (= (cadr r) (caddr m))
                   (not (member (cadddr m) sizediff)))
            (setq sizediff (cons (cadddr m) sizediff)))
          (setq merged (cons (cons (nth 7 r) r) merged))))
     )
     (setq rows (reverse merged))
     (if mixed
       (princ (strcat "\n注意: 同じ番号なのに名称(ブロック名・図形の種類)が違う対象があります →"
                      (apply 'strcat
                             (mapcar '(lambda (x) (strcat " " x))
                                     (reverse mixed))))))
     (if sizediff
       (princ (strcat "\n注意: 同じ番号でサイズが違う対象は、別の行に分けました →"
                      (apply 'strcat
                             (mapcar '(lambda (x) (strcat " " x))
                                     (reverse sizediff))))))
     ;; 分けて出力するかどうかを確認する。分ける場合は、属性「エリア」か「階数」の値ごとに
     ;; CSVを分ける(番号の英字部分では分けない)。わかれる値の列位置: エリア=10、階数=9
     (initget "None Area Floor")
     (setq s3 (getkword "\nタブ(ファイル)を分けますか? [分けない(N)/エリア(A)/階数(F)] <分けない>: "))
     (setq split (cond ((= s3 "Area") 10) ((= s3 "Floor") 9) (t nil)))
     (setq path (getfiled (if split
                            "保存先とファイル名(グループごとに _グループ名 が付いて保存されます)"
                            "保存先とファイル名")
                          (strcat (getvar "DWGPREFIX")
                                  (vl-filename-base (getvar "DWGNAME"))
                                  "_番号.csv")
                          "csv" 1))
     (if path
       (progn
         (if split
           (progn
             (setq gl nil)
             (foreach r rows
               (if (not (member (nth split r) gl))
                 (setq gl (append gl (list (nth split r))))))
             (setq groups gl))
           (setq groups (list nil)) ; 分けない場合は、全件をまとめた1グループ
         )
         (foreach grp groups
           (setq file (if split
                        (strcat (vl-filename-directory path) "/"
                                (vl-filename-base path) "_"
                                (num:safe (if (= grp "") "なし" grp)) ".csv")
                        path))
           (if (setq f (open file "w"))
             (progn
               (write-line "什器No,名称,幅(mm),奥行(mm),高さ(mm),個数,階数,エリア,什器分類,色,備考" f)
               (setq kinds 0 total 0)
               (foreach r rows
                 (if (or (not split) (= (nth split r) grp))
                   (progn
                     (write-line (strcat (cadddr r) ",\"" (nth 4 r) "\","
                                         (nth 5 r) "," (nth 6 r) "," (nth 7 r) ","
                                         (itoa (fix (+ (car r) 0.5))) ",\""
                                         (nth 9 r) "\",\"" (nth 10 r) "\",\"" (nth 13 r)
                                         "\",\"" (nth 11 r) "\",\"" (nth 12 r) "\"")
                                 f)
                     (setq kinds (1+ kinds) total (+ total (car r))))))
               (close f)
               (princ (strcat "\n" (if split (strcat (if (= grp "") "なし" grp) ": ") "")
                              (itoa kinds) " 種類 / 個数 " (itoa (fix (+ total 0.5))) " → " file)))
             (princ (strcat "\nファイルを開けませんでした(Excelで開いていませんか?): " file))))))
    )
  )
  (princ)
)

;;; ---- NUMA : 既存のブロックに、標準の属性を追加する -----------------

;; 追加する属性のタグ一覧(項目を変えたいときは、この一覧を書き換える)。
;; それぞれ、非表示・プリセット(ブロックを置くときに聞かれない)属性になる。
(setq *numa-tags* '("什器No" "階数" "エリア" "品名" "幅" "奥行" "高さ" "個数" "什器分類" "色" "備考"))

(defun c:NUMA ( / *error* doc oldecho ss i obj nm names added tot nblk)

  (defun *error* (msg)
    (if oldecho (setvar "CMDECHO" oldecho))
    (if doc (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nエラー: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *num-acache* nil)
  (princ "\n属性を追加するブロックを選択してください")
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
        (setq added (num:replace-tags nm *numa-tags*))
        (cond
          ((not added)
           (princ (strcat "\n" nm ": 外部参照のためスキップしました")))
          ((> added 0)
           (setq tot (+ tot added) nblk (1+ nblk))
           (princ (strcat "\n" nm ": " (itoa added) " 項目を追加して反映しました")))
          (t
           (princ (strcat "\n" nm ": すべて追加済みです")))
        )
      )
      (vla-EndUndoMark doc)
      (setvar "CMDECHO" oldecho)
      (princ (strcat "\n完了: " (itoa nblk) " 種類のブロックに、合計 " (itoa tot) " 項目を追加しました。"))
    )
    (princ "\nブロックが選択されませんでした。")
  )
  (princ)
)

;; ブロック定義nmに、tags(タグ名の一覧)のうち足りないものをすべて追加する。
;; 追加位置は num:blk-anchor の基準点から下方向へ200ピッチ、高さ125、非表示・プリセット属性。
;; 1つでも追加したら ATTSYNC で、図面にあるそのブロックに反映する。
;; 戻り値: 追加した項目数(0なら、すべて追加済み)。外部参照など処理できない場合は nil。
(defun num:ensure-tags (nm tags / def have e anc a tag idx added)
  (setq def (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) nm))
  (if (= (vla-get-IsXRef def) :vlax-true)
    nil
    (progn
      (setq have nil)
      (vlax-for e def
        (if (= (vla-get-ObjectName e) "AcDbAttributeDefinition")
          (setq have (cons (strcase (vla-get-TagString e)) have)))
      )
      ;; 外形が測れないブロック(属性だけなど)は、原点を基準点にする
      ;; NOTE: AutoLISPの or は値ではなく T/nil を返すため、ここでは or で値を選ばない
      (setq anc (num:blk-anchor nm))
      (if (not anc) (setq anc (list 0.0 0.0 0.0)))
      (setq added 0 idx (length have))
      (foreach tag tags
        (if (not (member (strcase tag) have))
          (progn
            (setq a (vl-catch-all-apply
                      'vla-AddAttribute
                      (list def 125.0 9 tag
                            (vlax-3d-point (list (car anc) (- (cadr anc) (* 200.0 idx)) 0.0))
                            tag "")))
            (if (not (vl-catch-all-error-p a))
              (progn
                (vla-put-Layer a "0")
                (setq have (cons (strcase tag) have))
                (setq added (1+ added)))
              (princ (strcat "\n" nm ": 「" tag "」を追加できませんでした")))
            (setq idx (1+ idx))
          )
        )
      )
      (if (> added 0) (command "_.ATTSYNC" "_N" nm))
      added
    )
  )
)

;; ブロック定義nmの属性定義を、tags(タグ名の一覧)で丸ごと置き換える。
;; 既存の属性定義は、タグ名が一致するかどうかに関わらず、いったんすべて削除してから、
;; tagsの項目を追加し直す(配置・高さは num:ensure-tags と同じ)。
;; 置き換えは、図面にあるそのブロックすべてに影響する(ATTSYNCで反映)。既存の値は失われる。
;; 戻り値: 追加した項目数。外部参照など処理できない場合は nil。
(defun num:replace-tags (nm tags / def olds e anc tag idx a added)
  (setq def (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) nm))
  (if (= (vla-get-IsXRef def) :vlax-true)
    nil
    (progn
      ;; 既存の属性定義を集めてから削除する(列挙しながらの削除は避ける)
      (setq olds nil)
      (vlax-for e def
        (if (= (vla-get-ObjectName e) "AcDbAttributeDefinition")
          (setq olds (cons e olds))))
      (foreach e olds (vl-catch-all-apply 'vla-Delete (list e)))
      (setq anc (num:blk-anchor nm))
      (if (not anc) (setq anc (list 0.0 0.0 0.0)))
      (setq added 0 idx 0)
      (foreach tag tags
        (setq a (vl-catch-all-apply
                  'vla-AddAttribute
                  (list def 125.0 9 tag
                        (vlax-3d-point (list (car anc) (- (cadr anc) (* 200.0 idx)) 0.0))
                        tag "")))
        (if (not (vl-catch-all-error-p a))
          (progn
            (vla-put-Layer a "0")
            (setq added (1+ added)))
          (princ (strcat "\n" nm ": 「" tag "」を追加できませんでした")))
        (setq idx (1+ idx))
      )
      (command "_.ATTSYNC" "_N" nm)
      added
    )
  )
)

;; strをdelimで分割した文字列のリストを返す
(defun num:split (str delim / out pos)
  (setq out nil)
  (while (setq pos (vl-string-search delim str))
    (setq out (cons (substr str 1 pos) out)
          str (substr str (+ pos 1 (strlen delim))))
  )
  (reverse (cons str out))
)


;; ============================================================
;;  PowerShell(WPF)ダイアログ関連のヘルパー
;;  QSコマンドと同じ方式(埋め込んだPowerShellスクリプトを一時ファイルに書き出し、
;;  WScript.Shell経由で実行する)。Windows + PowerShellが必要。AutoCADでは未検証。
;; ============================================================

;; strの中のoldをすべてnewに置き換える(num:splitと違い、全置換)
(defun num:replace-all (s old new / i out)
  (setq out "")
  (while (setq i (vl-string-search old s))
    (setq out (strcat out (substr s 1 i) new)
          s (substr s (+ i 1 (strlen old))))
  )
  (strcat out s)
)

;; sを、PowerShellのダブルクォート文字列の中に安全に入れられるようにする
;; (バッククォート・二重引用符・$ を処理する)
(defun num:ps-esc (s)
  (setq s (num:replace-all s "`" "``"))
  (setq s (num:replace-all s "\"" "'"))
  (setq s (num:replace-all s "$" "`$"))
  s
)

;; ファイルパス用(ダブルクォートの中に入れるだけなので、引用符だけ安全にする)
(defun num:ps-path (s)
  (num:replace-all s "\"" "'")
)

;; items(文字列のリスト)から、PowerShellの配列リテラルの中身を作る(カンマ区切り)
(defun num:ps-items (items / s)
  (setq s "")
  (foreach it items
    (setq s (strcat s (if (= s "") "" ",\n") "\"" (num:ps-esc it) "\"")))
  s
)

;; scriptを一時的な .ps1 として書き出し、WScript.Shell経由で実行する(閉じるまで待つ)
(defun num:run-ps (script / psPath wsh f)
  (setq psPath (vl-filename-mktemp "numui_" nil ".ps1"))
  (setq f (open psPath "w"))
  (write-line script f)
  (close f)
  (setq wsh (vlax-create-object "WScript.Shell"))
  (vl-catch-all-apply
    'vlax-invoke
    (list wsh 'Run
          (strcat "powershell.exe -NoProfile -ExecutionPolicy Bypass -WindowStyle Normal -File \"" psPath "\"")
          1 T))
  (vlax-release-object wsh)
  (if (findfile psPath) (vl-file-delete psPath))
  (princ)
)


;; アイテムを一覧から選ぶ、PowerShell(WPF)のチェックリストダイアログを表示する。
;; title = ウィンドウタイトル、items = 選択肢(文字列のリスト)。
;; 戻り値: チェックを付けて「OK」を押した項目のリスト(文字列)。
;; 「キャンセル」を押した、または何も選ばずOKを押した場合は nil。
;; 注意: Windows + PowerShell が必要。AutoCADでは未検証。
(defun num:wpf-checklist (title items / outPath itemsBlock script result f line)
  (setq outPath (num:ps-path (vl-filename-mktemp "numui_out_" nil ".txt")))
  (setq itemsBlock (num:ps-items items))
  (setq script
    (strcat
      "Add-Type -AssemblyName PresentationFramework\n"
      "Add-Type -AssemblyName PresentationCore\n"
      "\n"
      "$items = @(\n"
      itemsBlock "\n"
      ")\n"
      "$outFile = \"" outPath "\"\n"
      "$title = \"" (num:ps-esc title) "\"\n"
      "\n"
      "[xml]$xaml = @'\n"
      "<Window xmlns=\"http://schemas.microsoft.com/winfx/2006/xaml/presentation\"\n"
      "        xmlns:x=\"http://schemas.microsoft.com/winfx/2006/xaml\"\n"
      "        Height=\"680\" Width=\"780\" MinHeight=\"400\" MinWidth=\"560\"\n"
      "        WindowStartupLocation=\"CenterScreen\" ResizeMode=\"CanResize\"\n"
      "        Background=\"#F5F6F8\" Foreground=\"#1F2328\" FontFamily=\"Meiryo UI\" FontSize=\"14\"\n"
      "        UseLayoutRounding=\"True\" TextOptions.TextFormattingMode=\"Display\">\n"
      "  <Window.Resources>\n"
      "    <Style x:Key=\"Btn\" TargetType=\"Button\">\n"
      "      <Setter Property=\"Background\" Value=\"#FFFFFF\"/>\n"
      "      <Setter Property=\"Foreground\" Value=\"#344054\"/>\n"
      "      <Setter Property=\"BorderBrush\" Value=\"#D0D5DD\"/>\n"
      "      <Setter Property=\"Padding\" Value=\"16,7\"/>\n"
      "      <Setter Property=\"MinWidth\" Value=\"100\"/>\n"
      "      <Setter Property=\"Cursor\" Value=\"Hand\"/>\n"
      "      <Setter Property=\"Template\">\n"
      "        <Setter.Value>\n"
      "          <ControlTemplate TargetType=\"Button\">\n"
      "            <Border x:Name=\"bd\" Background=\"{TemplateBinding Background}\" BorderBrush=\"{TemplateBinding BorderBrush}\"\n"
      "                    BorderThickness=\"1\" CornerRadius=\"6\" Padding=\"{TemplateBinding Padding}\">\n"
      "              <ContentPresenter HorizontalAlignment=\"Center\" VerticalAlignment=\"Center\"/>\n"
      "            </Border>\n"
      "            <ControlTemplate.Triggers>\n"
      "              <Trigger Property=\"IsMouseOver\" Value=\"True\"><Setter TargetName=\"bd\" Property=\"Opacity\" Value=\"0.85\"/></Trigger>\n"
      "              <Trigger Property=\"IsEnabled\" Value=\"False\"><Setter TargetName=\"bd\" Property=\"Opacity\" Value=\"0.4\"/></Trigger>\n"
      "            </ControlTemplate.Triggers>\n"
      "          </ControlTemplate>\n"
      "        </Setter.Value>\n"
      "      </Setter>\n"
      "    </Style>\n"
      "    <Style x:Key=\"Primary\" TargetType=\"Button\" BasedOn=\"{StaticResource Btn}\">\n"
      "      <Setter Property=\"Background\" Value=\"#2563EB\"/>\n"
      "      <Setter Property=\"Foreground\" Value=\"#FFFFFF\"/>\n"
      "      <Setter Property=\"BorderBrush\" Value=\"#2563EB\"/>\n"
      "      <Setter Property=\"FontWeight\" Value=\"SemiBold\"/>\n"
      "    </Style>\n"
      "    <Style x:Key=\"Link\" TargetType=\"Button\" BasedOn=\"{StaticResource Btn}\">\n"
      "      <Setter Property=\"Background\" Value=\"Transparent\"/>\n"
      "      <Setter Property=\"BorderBrush\" Value=\"Transparent\"/>\n"
      "      <Setter Property=\"Foreground\" Value=\"#2563EB\"/>\n"
      "      <Setter Property=\"Padding\" Value=\"6,2\"/>\n"
      "      <Setter Property=\"MinWidth\" Value=\"0\"/>\n"
      "      <Setter Property=\"FontSize\" Value=\"13\"/>\n"
      "    </Style>\n"
      "  </Window.Resources>\n"
      "  <Grid>\n"
      "    <Grid.RowDefinitions>\n"
      "      <RowDefinition Height=\"Auto\"/>\n"
      "      <RowDefinition Height=\"Auto\"/>\n"
      "      <RowDefinition Height=\"Auto\"/>\n"
      "      <RowDefinition Height=\"*\"/>\n"
      "      <RowDefinition Height=\"Auto\"/>\n"
      "    </Grid.RowDefinitions>\n"
      "    <Border Grid.Row=\"0\" Background=\"#FFFFFF\" BorderBrush=\"#E4E7EC\" BorderThickness=\"0,0,0,1\" Padding=\"20,16\">\n"
      "      <StackPanel>\n"
      "        <TextBlock x:Name=\"TitleText\" FontSize=\"17\" FontWeight=\"SemiBold\" Foreground=\"#101828\" TextWrapping=\"Wrap\"/>\n"
      "        <TextBlock x:Name=\"CountText\" FontSize=\"13\" Foreground=\"#667085\" Margin=\"0,4,0,0\"/>\n"
      "      </StackPanel>\n"
      "    </Border>\n"
      "    <Border Grid.Row=\"1\" Margin=\"20,14,20,0\" Background=\"#FFFFFF\" BorderBrush=\"#D0D5DD\" BorderThickness=\"1\" CornerRadius=\"6\">\n"
      "      <Grid>\n"
      "        <TextBlock x:Name=\"Hint\" Text=\"キーワードで絞り込み\" Foreground=\"#98A2B3\" Margin=\"12,0\" VerticalAlignment=\"Center\" IsHitTestVisible=\"False\"/>\n"
      "        <TextBox x:Name=\"SearchBox\" Background=\"Transparent\" BorderThickness=\"0\" Padding=\"9,8\" VerticalContentAlignment=\"Center\" Foreground=\"#101828\"/>\n"
      "      </Grid>\n"
      "    </Border>\n"
      "    <StackPanel Grid.Row=\"2\" Orientation=\"Horizontal\" Margin=\"14,8,20,6\">\n"
      "      <Button x:Name=\"AllBtn\" Content=\"すべて選択\" Style=\"{StaticResource Link}\"/>\n"
      "      <Button x:Name=\"NoneBtn\" Content=\"すべて解除\" Style=\"{StaticResource Link}\" Margin=\"4,0,0,0\"/>\n"
      "    </StackPanel>\n"
      "    <Border Grid.Row=\"3\" Margin=\"14,0,14,0\">\n"
      "      <Grid>\n"
      "        <ScrollViewer VerticalScrollBarVisibility=\"Auto\">\n"
      "          <StackPanel x:Name=\"ItemsPanel\"/>\n"
      "        </ScrollViewer>\n"
      "        <TextBlock x:Name=\"EmptyText\" Text=\"該当する項目がありません\" Foreground=\"#98A2B3\"\n"
      "                   HorizontalAlignment=\"Center\" VerticalAlignment=\"Center\" Visibility=\"Collapsed\"/>\n"
      "      </Grid>\n"
      "    </Border>\n"
      "    <Border Grid.Row=\"4\" Margin=\"0,14,0,0\" Background=\"#FFFFFF\" BorderBrush=\"#E4E7EC\" BorderThickness=\"0,1,0,0\" Padding=\"20,12\">\n"
      "      <StackPanel Orientation=\"Horizontal\" HorizontalAlignment=\"Right\">\n"
      "        <Button x:Name=\"CancelBtn\" Content=\"キャンセル\" Style=\"{StaticResource Btn}\" IsCancel=\"True\"/>\n"
      "        <Button x:Name=\"OkBtn\" Content=\"OK\" Style=\"{StaticResource Primary}\" Margin=\"10,0,0,0\"/>\n"
      "      </StackPanel>\n"
      "    </Border>\n"
      "  </Grid>\n"
      "</Window>\n"
      "'@\n"
      "\n"
      "$reader = New-Object System.Xml.XmlNodeReader $xaml\n"
      "$window = [Windows.Markup.XamlReader]::Load($reader)\n"
      "$window.Title = $title\n"
      "foreach ($n in 'TitleText','CountText','Hint','SearchBox','AllBtn','NoneBtn','ItemsPanel','EmptyText','CancelBtn','OkBtn') {\n"
      "  Set-Variable -Name $n -Value $window.FindName($n) -Scope Script\n"
      "}\n"
      "$TitleText.Text = $title\n"
      "\n"
      "$bc = New-Object System.Windows.Media.BrushConverter\n"
      "$brWhite   = $bc.ConvertFromString('#FFFFFF')\n"
      "$brHover   = $bc.ConvertFromString('#F8FAFC')\n"
      "$brChecked = $bc.ConvertFromString('#EFF4FF')\n"
      "$brLine    = $bc.ConvertFromString('#EEF0F3')\n"
      "$brSub     = $bc.ConvertFromString('#667085')\n"
      "$brMain    = $bc.ConvertFromString('#101828')\n"
      "$brBadge   = $bc.ConvertFromString('#EAECF0')\n"
      "$brBadgeFg = $bc.ConvertFromString('#475467')\n"
      "\n"
      "$script:rows = @()\n"
      "\n"
      "$brGroup   = $bc.ConvertFromString('#F9FAFB')\n"
      "$brGroupFg = $bc.ConvertFromString('#344054')\n"
      "$script:groups = @()\n"
      "\n"
      "function Update-View {\n"
      "  $n = 0; $vis = 0\n"
      "  foreach ($r in $script:rows) {\n"
      "    if ($r.Tag.IsChecked -eq $true) { $n++ }\n"
      "    if ($r.Visibility -eq 'Visible') { $vis++ }\n"
      "  }\n"
      "  $CountText.Text = \"全 $($script:rows.Count) 件 ・ 選択中 $n 件\"\n"
      "  $OkBtn.IsEnabled = ($n -gt 0)\n"
      "  if ($vis -eq 0) { $EmptyText.Visibility = 'Visible' } else { $EmptyText.Visibility = 'Collapsed' }\n"
      "  # 項目ごとの見出し: 選択数と、ボタンの表示(全部選ばれていれば「解除」)を更新する\n"
      "  foreach ($g in $script:groups) {\n"
      "    $gn = 0; $gv = 0; $gvc = 0\n"
      "    foreach ($r in $g.Rows) {\n"
      "      if ($r.Tag.IsChecked -eq $true) { $gn++ }\n"
      "      if ($r.Visibility -eq 'Visible') { $gv++; if ($r.Tag.IsChecked -eq $true) { $gvc++ } }\n"
      "    }\n"
      "    $g.Count.Text = \"$gn / $($g.Rows.Count)\"\n"
      "    if ($gn -gt 0) { $g.Count.Foreground = $bc.ConvertFromString('#2563EB') } else { $g.Count.Foreground = $brSub }\n"
      "    if ($gv -gt 0 -and $gvc -eq $gv) { $g.Btn.Content = '解除' } else { $g.Btn.Content = 'すべて選択' }\n"
      "    if ($gv -eq 0) { $g.Panel.Visibility = 'Collapsed' } else { $g.Panel.Visibility = 'Visible' }\n"
      "  }\n"
      "}\n"
      "\n"
      "$brCardLine = $bc.ConvertFromString('#E4E7EC')\n"
      "$brCardOn   = $bc.ConvertFromString('#93B4F8')\n"
      "$script:flat = $null\n"
      "\n"
      "function Set-RowColor($row) {\n"
      "  if ($row.Tag.IsChecked -eq $true) { $row.Background = $brChecked; $row.BorderBrush = $brCardOn }\n"
      "  elseif ($row.IsMouseOver) { $row.Background = $brHover; $row.BorderBrush = $brCardLine }\n"
      "  else { $row.Background = $brWhite; $row.BorderBrush = $brCardLine }\n"
      "}\n"
      "\n"
      "# 項目名だけの一覧(NUMI)を2列に並べる入れ物(折り畳まれた行は詰めて並ぶ)\n"
      "function New-TwoCol {\n"
      "  $ug = New-Object System.Windows.Controls.Primitives.UniformGrid\n"
      "  $ug.Columns = 2\n"
      "  $ug.VerticalAlignment = 'Top'\n"
      "  $ug.Margin = '8,6,8,8'\n"
      "  return $ug\n"
      "}\n"
      "\n"
      "function Set-GroupOpen($g, $open) {\n"
      "  $g.Open = $open\n"
      "  if ($open) { $g.Body.Visibility = 'Visible'; $g.Arrow.Text = [string][char]0x25BE }\n"
      "  else { $g.Body.Visibility = 'Collapsed'; $g.Arrow.Text = [string][char]0x25B8 }\n"
      "}\n"
      "\n"
      "# 項目(タグ名)ごとの見出しと、その下に行を入れる入れ物を作る\n"
      "function New-Group($name) {\n"
      "  # 項目ごとに1枚のカードにする(カードは、あとで左右2列に振り分ける)\n"
      "  $card = New-Object System.Windows.Controls.Border\n"
      "  $card.Background = $brWhite\n"
      "  $card.BorderBrush = $brCardLine\n"
      "  $card.BorderThickness = '1'\n"
      "  $card.CornerRadius = '8'\n"
      "  $card.Margin = '6'\n"
      "  $panel = New-Object System.Windows.Controls.StackPanel\n"
      "  $card.Child = $panel\n"
      "  $hd = New-Object System.Windows.Controls.Border\n"
      "  $hd.CornerRadius = '8,8,0,0'\n"
      "  $hd.Background = $brGroup\n"
      "  $hd.BorderBrush = $brLine\n"
      "  $hd.BorderThickness = '0,0,0,1'\n"
      "  $hd.Padding = '12,7,8,7'\n"
      "  $hd.Cursor = 'Hand'\n"
      "  $hg = New-Object System.Windows.Controls.Grid\n"
      "  foreach ($w in 'Auto','*','Auto','Auto') {\n"
      "    $cd = New-Object System.Windows.Controls.ColumnDefinition; $cd.Width = $w; $hg.ColumnDefinitions.Add($cd)\n"
      "  }\n"
      "  $arrow = New-Object System.Windows.Controls.TextBlock\n"
      "  $arrow.Foreground = $brSub; $arrow.Width = 18; $arrow.VerticalAlignment = 'Center'\n"
      "  $nm = New-Object System.Windows.Controls.TextBlock\n"
      "  $nm.Text = $name; $nm.FontWeight = 'SemiBold'; $nm.Foreground = $brGroupFg; $nm.VerticalAlignment = 'Center'\n"
      "  $cnt = New-Object System.Windows.Controls.TextBlock\n"
      "  $cnt.FontSize = 12; $cnt.VerticalAlignment = 'Center'; $cnt.Margin = '8,0,4,0'\n"
      "  $btn = New-Object System.Windows.Controls.Button\n"
      "  $btn.Style = $window.FindResource('Link')\n"
      "  $btn.FontSize = 12\n"
      "  [System.Windows.Controls.Grid]::SetColumn($nm, 1)\n"
      "  [System.Windows.Controls.Grid]::SetColumn($cnt, 2)\n"
      "  [System.Windows.Controls.Grid]::SetColumn($btn, 3)\n"
      "  foreach ($c in $arrow,$nm,$cnt,$btn) { $hg.Children.Add($c) | Out-Null }\n"
      "  $hd.Child = $hg\n"
      "  $body = New-Object System.Windows.Controls.StackPanel\n"
      "  $body.Margin = '6'\n"
      "  $panel.Children.Add($hd) | Out-Null\n"
      "  $panel.Children.Add($body) | Out-Null\n"
      "  $g = @{ Name = $name; Panel = $card; Body = $body; Arrow = $arrow; Count = $cnt; Btn = $btn; Rows = @(); Open = $true }\n"
      "  $hd.Tag = $g\n"
      "  $btn.Tag = $g\n"
      "  # 見出しをクリックすると開閉、右のボタンでその項目の値をまとめて選択/解除\n"
      "  $hd.Add_MouseLeftButtonUp({ Set-GroupOpen $this.Tag (-not $this.Tag.Open) })\n"
      "  $btn.Add_Click({\n"
      "    $gg = $this.Tag\n"
      "    $on = ($this.Content -ne '解除')\n"
      "    foreach ($r in $gg.Rows) { if ($r.Visibility -eq 'Visible') { $r.Tag.IsChecked = $on } }\n"
      "  })\n"
      "  Set-GroupOpen $g $true\n"
      "  $script:groups += $g\n"
      "  return $g\n"
      "}\n"
      "\n"
      "foreach ($it in $items) {\n"
      "  $row = New-Object System.Windows.Controls.Border\n"
      "  $row.BorderBrush = $brCardLine\n"
      "  $row.BorderThickness = '1'\n"
      "  $row.CornerRadius = '6'\n"
      "  if ($it -match '^(.+?): (.*) [(]([0-9]+)件[)]$') { $row.Margin = '0,3' } else { $row.Margin = '4' }\n"
      "  $row.Padding = '10,8'\n"
      "  $row.Background = $brWhite\n"
      "  $row.Cursor = 'Hand'\n"
      "\n"
      "  $grid = New-Object System.Windows.Controls.Grid\n"
      "  $c0 = New-Object System.Windows.Controls.ColumnDefinition; $c0.Width = 'Auto'\n"
      "  $c1 = New-Object System.Windows.Controls.ColumnDefinition; $c1.Width = '*'\n"
      "  $c2 = New-Object System.Windows.Controls.ColumnDefinition; $c2.Width = 'Auto'\n"
      "  $grid.ColumnDefinitions.Add($c0); $grid.ColumnDefinitions.Add($c1); $grid.ColumnDefinitions.Add($c2)\n"
      "\n"
      "  $cb = New-Object System.Windows.Controls.CheckBox\n"
      "  $cb.Tag = $it\n"
      "  $cb.VerticalAlignment = 'Center'\n"
      "  $cb.Margin = '0,0,12,0'\n"
      "  $cb.LayoutTransform = New-Object System.Windows.Media.ScaleTransform 1.2, 1.2\n"
      "  [System.Windows.Controls.Grid]::SetColumn($cb, 0)\n"
      "  $grid.Children.Add($cb) | Out-Null\n"
      "\n"
      "  $main = New-Object System.Windows.Controls.TextBlock\n"
      "  $main.Foreground = $brMain\n"
      "  $main.TextWrapping = 'Wrap'\n"
      "  $main.VerticalAlignment = 'Center'\n"
      "  [System.Windows.Controls.Grid]::SetColumn($main, 1)\n"
      "  $grp = $null\n"
      "  # 「タグ: 値 (N件)」の形式なら、タグ名ごとの見出しの下に入れ、件数は右側のバッジにする\n"
      "  if ($it -match '^(.+?): (.*) [(]([0-9]+)件[)]$') {\n"
      "    $tagName = $Matches[1]; $main.Text = $Matches[2]; $num = $Matches[3]\n"
      "    $grp = $script:groups | Where-Object { $_.Name -eq $tagName } | Select-Object -First 1\n"
      "    if (-not $grp) { $grp = New-Group $tagName }\n"
      "    $badge = New-Object System.Windows.Controls.Border\n"
      "    $badge.Background = $brBadge\n"
      "    $badge.CornerRadius = '10'\n"
      "    $badge.Padding = '9,2'\n"
      "    $badge.Margin = '10,0,0,0'\n"
      "    $badge.VerticalAlignment = 'Center'\n"
      "    $bt = New-Object System.Windows.Controls.TextBlock\n"
      "    $bt.Text = \"$num 件\"\n"
      "    $bt.FontSize = 12\n"
      "    $bt.Foreground = $brBadgeFg\n"
      "    $badge.Child = $bt\n"
      "    [System.Windows.Controls.Grid]::SetColumn($badge, 2)\n"
      "    $grid.Children.Add($badge) | Out-Null\n"
      "  } else {\n"
      "    $main.Text = $it\n"
      "  }\n"
      "  $grid.Children.Add($main) | Out-Null\n"
      "\n"
      "  $row.Child = $grid\n"
      "  $row.Tag = $cb\n"
      "\n"
      "  # 行のどこをクリックしても、チェックを切り替える(チェックボックス自体のクリックはそのまま)\n"
      "  $row.Add_MouseLeftButtonUp({ $this.Tag.IsChecked = -not ($this.Tag.IsChecked -eq $true) })\n"
      "  $row.Add_MouseEnter({ Set-RowColor $this })\n"
      "  $row.Add_MouseLeave({ Set-RowColor $this })\n"
      "  $cb.Add_Checked({ Set-RowColor $this.Parent.Parent; Update-View })\n"
      "  $cb.Add_Unchecked({ Set-RowColor $this.Parent.Parent; Update-View })\n"
      "\n"
      "  if ($grp) {\n"
      "    $grp.Body.Children.Add($row) | Out-Null\n"
      "    $grp.Rows += $row\n"
      "  } else {\n"
      "    if (-not $script:flat) { $script:flat = New-TwoCol; $ItemsPanel.Children.Add($script:flat) | Out-Null }\n"
      "    $script:flat.Children.Add($row) | Out-Null\n"
      "  }\n"
      "  $script:rows += $row\n"
      "}\n"
      "\n"
      "# 項目のカードを左右2列に振り分ける(行数の少ない方の列へ、順に入れていく)\n"
      "if ($script:groups.Count -gt 0) {\n"
      "  $cols = New-Object System.Windows.Controls.Grid\n"
      "  $cols.Margin = '0,0,0,6'\n"
      "  $cd0 = New-Object System.Windows.Controls.ColumnDefinition; $cd0.Width = '*'\n"
      "  $cd1 = New-Object System.Windows.Controls.ColumnDefinition; $cd1.Width = '*'\n"
      "  $cols.ColumnDefinitions.Add($cd0); $cols.ColumnDefinitions.Add($cd1)\n"
      "  $left = New-Object System.Windows.Controls.StackPanel\n"
      "  $right = New-Object System.Windows.Controls.StackPanel\n"
      "  [System.Windows.Controls.Grid]::SetColumn($right, 1)\n"
      "  $cols.Children.Add($left) | Out-Null\n"
      "  $cols.Children.Add($right) | Out-Null\n"
      "  $hl = 0; $hr = 0\n"
      "  foreach ($g in $script:groups) {\n"
      "    $size = $g.Rows.Count + 2\n"
      "    if ($hl -le $hr) { $left.Children.Add($g.Panel) | Out-Null; $hl += $size }\n"
      "    else { $right.Children.Add($g.Panel) | Out-Null; $hr += $size }\n"
      "  }\n"
      "  $ItemsPanel.Children.Add($cols) | Out-Null\n"
      "}\n"
      "\n"
      "$SearchBox.Add_TextChanged({\n"
      "  $f = $SearchBox.Text\n"
      "  if ($f -eq '') { $Hint.Visibility = 'Visible' } else { $Hint.Visibility = 'Collapsed' }\n"
      "  foreach ($r in $script:rows) {\n"
      "    if ($f -eq '' -or $r.Tag.Tag.IndexOf($f, [System.StringComparison]::OrdinalIgnoreCase) -ge 0) {\n"
      "      $r.Visibility = 'Visible'\n"
      "    } else {\n"
      "      $r.Visibility = 'Collapsed'\n"
      "    }\n"
      "  }\n"
      "  # 絞り込み中は、該当する値がある項目を開いて見せる\n"
      "  if ($f -ne '') { foreach ($g in $script:groups) { Set-GroupOpen $g $true } }\n"
      "  Update-View\n"
      "})\n"
      "\n"
      "# すべて選択・すべて解除は、絞り込みで表示されている項目だけが対象\n"
      "$AllBtn.Add_Click({ foreach ($r in $script:rows) { if ($r.Visibility -eq 'Visible') { $r.Tag.IsChecked = $true } } })\n"
      "$NoneBtn.Add_Click({ foreach ($r in $script:rows) { if ($r.Visibility -eq 'Visible') { $r.Tag.IsChecked = $false } } })\n"
      "\n"
      "$OkBtn.Add_Click({\n"
      "  $checked = @()\n"
      "  foreach ($r in $script:rows) { if ($r.Tag.IsChecked -eq $true) { $checked += [string]$r.Tag.Tag } }\n"
      "  $enc = New-Object System.Text.UTF8Encoding $false\n"
      "  [System.IO.File]::WriteAllLines($outFile, [string[]]$checked, $enc)\n"
      "  $window.Close()\n"
      "})\n"
      "$CancelBtn.Add_Click({ $window.Close() })\n"
      "\n"
      "$window.Add_ContentRendered({ $SearchBox.Focus() | Out-Null })\n"
      "Update-View\n"
      "$window.ShowDialog() | Out-Null\n"
    )
  )
  (num:run-ps script)
  (setq result nil)
  (if (findfile outPath)
    (progn
      (setq f (open outPath "r"))
      (while (setq line (read-line f))
        (if (/= (vl-string-trim " " line) "") (setq result (cons line result))))
      (close f)
      (setq result (reverse result))
      (vl-file-delete outPath)
    )
  )
  result
)


;; lst の中で item と等しい最初の要素のインデックスを返す(なければ nil)
(defun num:index-of (item lst / i found)
  (setq i 0 found nil)
  (while (and (< i (length lst)) (not found))
    (if (equal (nth i lst) item) (setq found i))
    (setq i (1+ i))
  )
  found
)

;; 重複を取り除いたリストを返す(順序は保つ)
(defun num:uniq (lst / r)
  (foreach x lst (if (not (member x r)) (setq r (cons x r))))
  (reverse r)
)

;;; ---- NUMC : 属性の値を、他のブロックにコピーする -------------------

;; NUMCでコピーしないタグ(対象ごとに固有の値として残す。変えたいときはここを書き換える)。
(setq *numc-skip* '("什器No"))

;; コピー元のブロックから、属性のタグ名(元の表記のまま)の一覧を返す
(defun num:att-tags (obj / lst a)
  (if (= (vla-get-HasAttributes obj) :vlax-true)
    (foreach a (vlax-invoke obj 'GetAttributes)
      (setq lst (cons (vla-get-TagString a) lst)))
  )
  (reverse lst)
)

(defun c:NUMC ( / *error* doc s0 sh sObj satts sname stags ss i tEnt tObj a tag v cnt objs skipped skiptags tcnt mism nm2 added pre targets)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nエラー: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; コマンドの実行前にすでに選択されていれば、それを使う。
  ;; 選択の1個目をコピー元にする。2個以上選んでいれば、残りはコピー先にする
  ;; (1個だけの場合は、コピー先はこのあと改めて選んでもらう)。
  (setq pre (ssget "_I" '((0 . "INSERT"))))
  (sssetfirst nil nil) ; 選択(グリップ)を解除しておく
  (if pre
    (progn
      (setq s0 (list (ssname pre 0)))
      (setq targets nil i 1)
      (repeat (1- (sslength pre))
        (setq targets (cons (ssname pre i) targets) i (1+ i)))
      (setq targets (reverse targets))
      (princ (strcat "\n選択済みの1個目をコピー元にします"
                     (if targets
                       (strcat "(残り " (itoa (length targets)) " 個をコピー先にします)")
                       ""))))
    (progn
      (setq s0 (entsel "\nコピー元のブロック(属性が入っているもの)を選択: "))
      (setq targets nil))
  )

  (cond
    ((not s0)
     (princ "\n対象が選択されませんでした。"))
    ((/= (cdr (assoc 0 (entget (car s0)))) "INSERT")
     (princ "\nブロックを選択してください。"))
    (t
     (setq sh    (cdr (assoc 5 (entget (car s0))))
           sObj  (vlax-ename->vla-object (car s0))
           sname (vla-get-EffectiveName sObj)
           satts (num:atts sObj)
           stags (num:att-tags sObj))
     (cond
       ((not satts)
        (princ "\nこのブロックには属性がありません。"))
       (t
        (setq skiptags (mapcar 'strcase *numc-skip*))
        ;; 事前選択がなかった場合だけ、ここでコピー先を選んでもらう
        (if (not targets)
          (progn
            (princ "\nコピー先のブロックを選択(複数可・窓選択も可)してください")
            (if (setq ss (ssget '((0 . "INSERT"))))
              (progn
                (setq targets nil i 0)
                (repeat (sslength ss)
                  (setq targets (cons (ssname ss i) targets) i (1+ i)))
                (setq targets (reverse targets))))))
        (cond
          ((not targets)
           (princ "\nコピー先が選択されませんでした。"))
          (t
           (setq cnt 0 objs 0 skipped 0 mism nil)
           (foreach tEnt targets
             (cond
               ;; コピー先の選択に、コピー元自身が含まれていた場合は除く
               ((= (cdr (assoc 5 (entget tEnt))) sh)
                (setq skipped (1+ skipped)))
               ;; ブロック以外(事前選択に紛れ込んだ図形など)は対象外
               ((/= (cdr (assoc 0 (entget tEnt))) "INSERT")
                (setq skipped (1+ skipped)))
               (t
                (setq tObj (vlax-ename->vla-object tEnt)
                      nm2  (vla-get-EffectiveName tObj))
                ;; もとの属性(コピー元とは無関係なものも含む)は削除し、コピー元と同じ項目を
                ;; 作り直す(ブロック定義ごと=そのブロックの他のインスタンスにも反映される)
                (setq added (num:replace-tags nm2 stags))
                (if added
                  (setq tObj (vlax-ename->vla-object tEnt))) ; ATTSYNC後の状態を取り直す
                (cond
                  ((and added (= (vla-get-HasAttributes tObj) :vlax-true))
                   (setq objs (1+ objs) tcnt 0)
                   (foreach a (vlax-invoke tObj 'GetAttributes)
                     (setq tag (strcase (vla-get-TagString a)))
                     (if (and (not (member tag skiptags))
                              (setq v (cdr (assoc tag satts))))
                       (progn
                         (vla-put-TextString a v)
                         (setq cnt (1+ cnt) tcnt (1+ tcnt))))
                   )
                   ;; 一致するタグが1つもなかったブロック名を記録する(あとで原因を示すため)
                   (if (= tcnt 0)
                     (setq mism (cons nm2 mism))))
                  (t
                   ;; added が nil(外部参照など)、または属性を追加・取得できなかった場合
                   (setq skipped (1+ skipped))))))
           )
           (princ (strcat "\n" (itoa objs) " 個のブロックに、合計 " (itoa cnt) " 項目をコピーしました。"
                          (if (> skipped 0)
                            (strcat "(対象外 " (itoa skipped) " 個)")
                            "")))
           ;; 1件もコピーできなかったブロックがあれば、原因がわかるようタグを一覧表示する
           (if mism
             (progn
               (princ (strcat "\n一致するタグがなく、コピーできなかったブロック: "
                              (apply (function strcat)
                                     (mapcar (function (lambda (x) (strcat x " ")))
                                             (num:uniq mism)))))
               (princ (strcat "\nコピー元(" sname ")のタグ: "
                              (apply (function strcat)
                                     (mapcar (function (lambda (p) (strcat (car p) " ")))
                                             satts))))
             ))
          )
        )
       )
     )
    )
  )
  (princ)
)

;;; ---- NUMF : 属性の項目と値で検索する -------------------------

(defun c:NUMF ( / *error* doc ss i ent obj a tg val key p pairs cnt sp
                  labels keys chosen chosenKeys idx matches ss2 nm first)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nエラー: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; 図面にある、属性を持つブロックから、「タグ+値」の組み合わせと件数を集める
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (setq pairs nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i) i (1+ i))
        (setq obj (vlax-ename->vla-object ent))
        (if (= (vla-get-HasAttributes obj) :vlax-true)
          (foreach a (vlax-invoke obj 'GetAttributes)
            (setq tg (vla-get-TagString a) val (vla-get-TextString a))
            (if (/= (vl-string-trim " " val) "")
              (progn
                (setq key (strcat tg (chr 1) val))
                (setq p (assoc key pairs))
                (if p
                  (setq pairs (subst (cons key (1+ (cdr p))) p pairs))
                  (setq pairs (cons (cons key 1) pairs)))
              )
            )
          )
        )
      )
    )
  )

  (cond
    ((not pairs)
     (princ "\n属性を持つブロックが見つかりません。"))
    (t
     ;; タグ名→値の順で並べ、「タグ: 値 (N件)」の一覧と、対応するキーの一覧を作る
     (setq pairs (vl-sort pairs (function (lambda (a b) (< (car a) (car b))))))
     (setq labels nil keys nil)
     (foreach p pairs
       (setq key (car p) cnt (cdr p))
       (setq sp (vl-string-search (chr 1) key))
       (setq tg (substr key 1 sp) val (substr key (+ sp 2)))
       (setq labels (cons (strcat tg ": " val " (" (itoa cnt) "件)") labels))
       (setq keys (cons key keys))
     )
     (setq labels (reverse labels) keys (reverse keys))

     ;; PowerShellのダイアログで、検索したい「タグ: 値」を選んでもらう(複数可)
     (setq chosen (num:wpf-checklist "検索する項目・値を選択" labels))

     (cond
       ((not chosen)
        (princ "\nキャンセルされたか、何も選ばれませんでした。"))
       (t
        ;; 選んだ表示名から、対応するキー(タグ+値)を逆引きする
        (setq chosenKeys nil)
        (foreach c chosen
          (setq idx (num:index-of c labels))
          (if idx (setq chosenKeys (cons (nth idx keys) chosenKeys)))
        )
        ;; いずれかの選んだ組み合わせを持つブロックを集める(1つのブロックは1回だけ数える)
        (setq matches nil i 0)
        (repeat (sslength ss)
          (setq ent (ssname ss i) i (1+ i))
          (setq obj (vlax-ename->vla-object ent))
          (if (= (vla-get-HasAttributes obj) :vlax-true)
            (foreach a (vlax-invoke obj 'GetAttributes)
              (setq tg (vla-get-TagString a) val (vla-get-TextString a))
              (if (and (member (strcat tg (chr 1) val) chosenKeys)
                       (not (member ent matches)))
                (setq matches (cons ent matches)))
            )
          )
        )
        (setq matches (reverse matches))
        (cond
          ((not matches)
           (princ "\n一致するものが見つかりませんでした。"))
          (t
           ;; 見つかった対象を、図面上で選択状態にする(続けてNUMCなどに使える)
           (setq ss2 (ssadd))
           (foreach ent matches (ssadd ent ss2))
           (sssetfirst nil ss2)
           (princ (strcat "\n" (itoa (length matches)) " 件見つかりました(図面上で選択状態にしました):"))
           (foreach ent matches
             (setq obj (vlax-ename->vla-object ent)
                   nm  (vla-get-EffectiveName obj)
                   first T)
             (princ (strcat "\n ・" nm))
             (foreach a (vlax-invoke obj 'GetAttributes)
               (setq tg (vla-get-TagString a) val (vla-get-TextString a))
               (if (member (strcat tg (chr 1) val) chosenKeys)
                 (progn
                   (princ (strcat (if first "  " ", ") tg "=" val))
                   (setq first nil))
               )
             )
           )
          )
        )
       )
     )
    )
  )
  (princ)
)

;;; ---- NUMI : 属性の項目を選んでコピーする(PowerShellダイアログ版) ----

(defun c:NUMI ( / *error* doc s0 sh sObj satts sname stags choices chosen chosenU
                  ss i tEnt tObj a tag v cnt objs skipped tcnt mism nm2 added pre targets)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nエラー: " msg))
    )
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; コマンドの実行前にすでに選択されていれば、それを使う。
  ;; 選択の1個目をコピー元にする。2個以上選んでいれば、残りはコピー先にする
  ;; (1個だけの場合は、コピー先はこのあと改めて選んでもらう)。
  (setq pre (ssget "_I" '((0 . "INSERT"))))
  (sssetfirst nil nil)
  (if pre
    (progn
      (setq s0 (list (ssname pre 0)))
      (setq targets nil i 1)
      (repeat (1- (sslength pre))
        (setq targets (cons (ssname pre i) targets) i (1+ i)))
      (setq targets (reverse targets))
      (princ (strcat "\n選択済みの1個目をコピー元にします"
                     (if targets
                       (strcat "(残り " (itoa (length targets)) " 個をコピー先にします)")
                       ""))))
    (progn
      (setq s0 (entsel "\nコピー元のブロック(属性が入っているもの)を選択: "))
      (setq targets nil))
  )

  (cond
    ((not s0)
     (princ "\n対象が選択されませんでした。"))
    ((/= (cdr (assoc 0 (entget (car s0)))) "INSERT")
     (princ "\nブロックを選択してください。"))
    (t
     (setq sh    (cdr (assoc 5 (entget (car s0))))
           sObj  (vlax-ename->vla-object (car s0))
           sname (vla-get-EffectiveName sObj)
           satts (num:atts sObj)
           stags (num:att-tags sObj))
     (cond
       ((not satts)
        (princ "\nこのブロックには属性がありません。"))
       (t
        ;; コピーできる項目(什器Noは常に除く)を、PowerShellのダイアログで選んでもらう
        (setq choices nil)
        (foreach tag stags
          (if (not (member (strcase tag) (mapcar 'strcase *numc-skip*)))
            (setq choices (cons tag choices))))
        (setq choices (reverse choices))
        (setq chosen (num:wpf-checklist (strcat "コピーする項目(" sname ")") choices))
        (cond
          ((not chosen)
           (princ "\nキャンセルされたか、項目が選ばれませんでした。"))
          (t
           (setq chosenU (mapcar 'strcase chosen))
           (princ (strcat "\nコピーする項目: "
                          (apply (function strcat) (mapcar (function (lambda (x) (strcat x " "))) chosen))))
           ;; 事前選択がなかった場合だけ、ここでコピー先を選んでもらう
           (if (not targets)
             (progn
               (princ "\nコピー先のブロックを選択(複数可・窓選択も可)してください")
               (if (setq ss (ssget '((0 . "INSERT"))))
                 (progn
                   (setq targets nil i 0)
                   (repeat (sslength ss)
                     (setq targets (cons (ssname ss i) targets) i (1+ i)))
                   (setq targets (reverse targets))))))
           (cond
             ((not targets)
              (princ "\nコピー先が選択されませんでした。"))
             (t
              (setq cnt 0 objs 0 skipped 0 mism nil)
              (foreach tEnt targets
                (cond
                  ((= (cdr (assoc 5 (entget tEnt))) sh)
                   (setq skipped (1+ skipped)))
                  ((/= (cdr (assoc 0 (entget tEnt))) "INSERT")
                   (setq skipped (1+ skipped)))
                  (t
                   (setq tObj (vlax-ename->vla-object tEnt)
                         nm2  (vla-get-EffectiveName tObj))
                   ;; 選んだ項目のうち、このブロックにまだないものだけを追加する(もとからある、
                   ;; 関係のない属性には触れない)。足りた項目は、値をコピー元で置き換える。
                   (setq added (num:ensure-tags nm2 chosen))
                   (if (and added (> added 0))
                     (setq tObj (vlax-ename->vla-object tEnt)))
                   (cond
                     ((and added (= (vla-get-HasAttributes tObj) :vlax-true))
                      (setq objs (1+ objs) tcnt 0)
                      (foreach a (vlax-invoke tObj 'GetAttributes)
                        (setq tag (strcase (vla-get-TagString a)))
                        (if (and (member tag chosenU)
                                 (setq v (cdr (assoc tag satts))))
                          (progn
                            (vla-put-TextString a v)
                            (setq cnt (1+ cnt) tcnt (1+ tcnt))))
                      )
                      (if (= tcnt 0)
                        (setq mism (cons nm2 mism))))
                     (t
                      (setq skipped (1+ skipped))))))
              )
              (princ (strcat "\n" (itoa objs) " 個のブロックに、合計 " (itoa cnt) " 項目をコピーしました。"
                             (if (> skipped 0)
                               (strcat "(対象外 " (itoa skipped) " 個)")
                               "")))
              (if mism
                (princ (strcat "\n選んだ項目が1つもコピーできなかったブロック: "
                               (apply (function strcat)
                                      (mapcar (function (lambda (x) (strcat x " ")))
                                              (num:uniq mism))))))
             )
           )
          )
        )
       )
     )
    )
  )
  (princ)
)

(princ "\nNUM(番号を付ける) / NUMX(個数つきCSV書き出し) / NUMA(ブロックに属性を追加) / NUMC(属性のコピー) / NUMF(属性の検索) / NUMI(項目選択コピー) を読み込みました。")
(princ)