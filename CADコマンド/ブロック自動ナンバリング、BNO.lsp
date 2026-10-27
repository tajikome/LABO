;;; ============================================================
;;; BNO.lsp  ブロック自動ナンバリング (IJCAD / AutoCAD 共通)
;;;   コマンド: BNO
;;;   ・窓選択したブロックを、指定した並び順で自動採番
;;;   ・番号の付け方: 1=全て別番号 / 2=同名ブロックは同じ番号 / 3=全て同じ番号
;;;   ・出力先: 文字(TEXT)を新規作成[初期値: 画層ナンバリング/ASA/高さ400/幅係数0.7/MC/線種ByLayer] または ブロック属性
;;;   ・設定と前回の最終番号を記憶 (getenv/setenv。使えない環境ではセッション内のみ)
;;; ============================================================
(vl-load-com)

(setq *bno:keys* '(mode pdir sdir tol step prefix suffix digits out tag th ox oy layer last style wf ltype lts ref))
(setq *bno:dn* '(("D" . "上から下") ("U" . "下から上") ("R" . "左から右") ("L" . "右から左")))

;;; ---------- 設定の初期値・保存・読込 ----------
(defun bno:defaults ()
  (list (cons 'mode 1) (cons 'pdir "R") (cons 'sdir "D") (cons 'tol 50.0)
        (cons 'step 1) (cons 'prefix "") (cons 'suffix "") (cons 'digits 0)
        (cons 'out "T") (cons 'tag "NO") (cons 'th 400.0) (cons 'ox 0.0)
        (cons 'oy 0.0) (cons 'layer "ナンバリング") (cons 'last 0)
        (cons 'style "ASA") (cons 'wf 0.7) (cons 'ltype "")
        (cons 'lts 3.0) (cons 'ref "C")))

(defun bno:split (s d / p r)
  (while (setq p (vl-string-search d s))
    (setq r (cons (substr s 1 p) r)
          s (substr s (+ p 1 (strlen d)))))
  (reverse (cons s r)))

(defun bno:join (lst d / r)
  (setq r (car lst))
  (foreach x (cdr lst) (setq r (strcat r d x)))
  r)

(defun bno:decode (vals / ks r k v)
  (setq ks *bno:keys*)
  (while ks
    (setq k (car ks) v (car vals))
    (setq r (cons (cons k (cond ((member k '(mode step digits last)) (atoi v))
                                ((member k '(tol th ox oy wf lts)) (atof v))
                                (t v)))
                  r))
    (setq ks (cdr ks) vals (cdr vals)))
  (reverse r))

(defun bno:load ( / s vals)
  (if (null *bno:cfg*)
    (progn
      (setq *bno:cfg* (bno:defaults))
      (setq s (vl-catch-all-apply 'getenv '("BNO_CFG")))
      (if (and s (not (vl-catch-all-error-p s)) (/= s ""))
        (progn
          (setq vals (bno:split s "|"))
          (if (= (length vals) (length *bno:keys*))
            (setq *bno:cfg* (bno:decode vals)))))))
  *bno:cfg*)

(defun bno:save ()
  (vl-catch-all-apply
    'setenv
    (list "BNO_CFG"
          (bno:join
            (mapcar '(lambda (k / v)
                       (setq v (cdr (assoc k *bno:cfg*)))
                       (cond ((= (type v) 'INT) (itoa v))
                             ((= (type v) 'REAL) (rtos v 2 4))
                             (t v)))
                    *bno:keys*)
            "|"))))

(defun bno:get (k) (cdr (assoc k *bno:cfg*)))
(defun bno:set (k v)
  (setq *bno:cfg* (subst (cons k v) (assoc k *bno:cfg*) *bno:cfg*)))
(defun bno:clean (s) (vl-string-translate "|" " " s))
(defun bno:dname (d) (cdr (assoc d *bno:dn*)))

;;; ---------- 番号の書式 ----------
(defun bno:fmt (n / s)
  (setq s (itoa n))
  (while (< (strlen s) (bno:get 'digits)) (setq s (strcat "0" s)))
  (strcat (bno:get 'prefix) s (bno:get 'suffix)))

;;; ---------- 並べ替え ----------
(defun bno:coord (pt dir)
  (cond ((= dir "R") (car pt))
        ((= dir "L") (- (car pt)))
        ((= dir "U") (cadr pt))
        (t (- (cadr pt)))))

(defun bno:sortby (lst keyfn / idx)
  (if lst
    (mapcar '(lambda (i) (nth i lst))
            (vl-sort-i (mapcar keyfn lst) '<))))

;; items: (ename p s)  p=行内の進行方向の座標 / s=行を移る方向の座標
(defun bno:order (items tol / sorted rows row base)
  (setq sorted (bno:sortby items '(lambda (a) (caddr a))))
  (foreach it sorted
    (if (and row (<= (- (caddr it) base) tol))
      (setq row (cons it row))
      (progn
        (if row (setq rows (cons row rows)))
        (setq row (list it) base (caddr it)))))
  (if row (setq rows (cons row rows)))
  (apply 'append
         (mapcar '(lambda (r) (bno:sortby (reverse r) '(lambda (a) (cadr a))))
                 (reverse rows))))

;;; ---------- 番号割り当て ----------
(defun bno:bname (en / o r)
  (setq o (vlax-ename->vla-object en))
  (setq r (vl-catch-all-apply 'vla-get-effectivename (list o)))
  (if (vl-catch-all-error-p r) (cdr (assoc 2 (entget en))) r))

(defun bno:assign (ens start / n step mode res names nm hit)
  (setq n start step (bno:get 'step) mode (bno:get 'mode))
  (foreach en ens
    (cond
      ((= mode 1)
       (setq res (cons (cons en n) res) n (+ n step)))
      ((= mode 3)
       (setq res (cons (cons en start) res)))
      (t
       (setq nm (strcase (bno:bname en)))
       (if (setq hit (assoc nm names))
         (setq res (cons (cons en (cdr hit)) res))
         (setq names (cons (cons nm n) names)
               res (cons (cons en n) res)
               n (+ n step))))))
  (reverse res))

;;; ---------- 書き込み ----------
;; 属性に書込。タグが無ければ nil
(defun bno:setattr (en tag val / nx ed done)
  (setq nx (entnext en))
  (while (and nx (not done)
              (= (cdr (assoc 0 (setq ed (entget nx)))) "ATTRIB"))
    (if (= (strcase (cdr (assoc 2 ed))) (strcase tag))
      (progn
        (entmod (subst (cons 1 val) (assoc 1 ed) ed))
        (entupd en)
        (setq done T)))
    (setq nx (entnext nx)))
  done)

;; 文字作成用: 過去にBNOが作った文字(所有ブロックのハンドルをXDATAに保持)
(defun bno:xdh (en / x)
  (setq x (assoc -3 (entget en '("BNO"))))
  (if x (cdr (assoc 1000 (cdr (cadr x))))))

(defun bno:oldtexts ( / ss i en h r)
  (if (setq ss (ssget "_X" (list '(0 . "TEXT") '(-3 ("BNO")) (cons 410 (getvar "CTAB")))))
    (repeat (setq i (sslength ss))
      (setq en (ssname ss (setq i (1- i))))
      (setq h (bno:xdh en))
      (if h (setq r (cons (cons h en) r)))))
  r)

(defun bno:center (en / o mn mx r)
  (setq o (vlax-ename->vla-object en))
  (setq r (vl-catch-all-apply '(lambda () (vla-getboundingbox o 'mn 'mx))))
  (if (vl-catch-all-error-p r)
    nil
    (progn
      (setq mn (vlax-safearray->list mn) mx (vlax-safearray->list mx))
      (list (/ (+ (car mn) (car mx)) 2.0)
            (/ (+ (cadr mn) (cadr mx)) 2.0)
            (caddr (cdr (assoc 10 (entget en))))))))

;; 文字作成の事前準備 (画層・線種が無ければ用意)
(defun bno:prep ( / lay lt)
  (setq lay (bno:get 'layer) lt (bno:get 'ltype))
  (if (and (/= lay "") (not (tblsearch "LAYER" lay)))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 lay) '(70 . 0) '(62 . 4) '(6 . "Continuous"))))
  (if (and (/= lt "") (not (tblsearch "LTYPE" lt)))
    (vl-catch-all-apply
      '(lambda () (command "_.-linetype" "_load" lt "acad.lin" "")))))

;; 文字(中央MC)を作成。位置=ブロック中心または挿入点 + ずらし
(defun bno:mktext (en val old / ed p h lay o sty lt)
  (setq ed (entget en) h (cdr (assoc 5 ed)))
  (setq p (if (= (bno:get 'ref) "C") (bno:center en)))
  (if (null p) (setq p (cdr (assoc 10 ed))))
  (setq p (list (+ (car p) (bno:get 'ox)) (+ (cadr p) (bno:get 'oy)) (caddr p)))
  (if (setq o (assoc h old))
    (vl-catch-all-apply 'entdel (list (cdr o))))
  (setq lay (bno:get 'layer) sty (bno:get 'style) lt (bno:get 'ltype))
  (if (or (= lay "") (not (tblsearch "LAYER" lay))) (setq lay (getvar "CLAYER")))
  (if (not (tblsearch "STYLE" sty)) (setq sty (getvar "TEXTSTYLE")))
  (entmake
    (append
      (list '(0 . "TEXT")
            (cons 8 lay)
            (cons 10 p)
            (cons 40 (bno:get 'th))
            (cons 1 val)
            (cons 41 (bno:get 'wf))
            (cons 7 sty)
            '(72 . 1)
            (cons 11 p)
            '(73 . 2))
      (list (cons 48 (bno:get 'lts)))
      (if (and (/= lt "") (tblsearch "LTYPE" lt))
        (list (cons 6 lt)))
      (list (list -3 (list "BNO" (cons 1000 h)))))))

;;; ---------- 入力ヘルパー ----------
(defun bno:askstr (msg key / def s)
  (setq def (bno:get key))
  (setq s (getstring T (strcat "\n" msg " <" (if (= def "") "なし" def) "> (.=なし): ")))
  (cond ((= s "") nil)
        ((= s ".") (bno:set key ""))
        (t (bno:set key (bno:clean s)))))

(defun bno:askint (msg key bits / v)
  (if (> bits 0) (initget bits))
  (setq v (getint (strcat "\n" msg " <" (itoa (bno:get key)) ">: ")))
  (if v (bno:set key v)))

(defun bno:askreal (msg key bits / v)
  (if (> bits 0) (initget bits))
  (setq v (getreal (strcat "\n" msg " <" (rtos (bno:get key) 2 2) ">: ")))
  (if v (bno:set key v)))

;;; ---------- 設定表示・変更 ----------
(defun bno:show ()
  (princ "\n------ BNO 現在の設定 ------")
  (princ (strcat "\n番号の付け方 : "
                 (nth (1- (bno:get 'mode))
                      '("全て別番号" "同名ブロックは同じ番号" "全て同じ番号"))))
  (princ (strcat "\n並び順       : 各行を" (bno:dname (bno:get 'pdir))
                 " → 次の行は" (bno:dname (bno:get 'sdir))
                 " (行の許容差 " (rtos (bno:get 'tol) 2 2) ")"))
  (princ (strcat "\n番号の書式   : 例 " (bno:fmt (+ (bno:get 'last) (bno:get 'step)))
                 "  (増分 " (itoa (bno:get 'step)) ")"))
  (princ (strcat "\n出力先       : "
                 (if (= (bno:get 'out) "A")
                   (strcat "ブロック属性 [タグ " (bno:get 'tag) "]")
                   (strcat "文字を作成 [画層 " (if (= (bno:get 'layer) "") "現在画層" (bno:get 'layer))
                           " / スタイル " (bno:get 'style)
                           " / 高さ " (rtos (bno:get 'th) 2 2)
                           " / 幅係数 " (rtos (bno:get 'wf) 2 2)
                           " / 線種 " (if (= (bno:get 'ltype) "") "ByLayer" (bno:get 'ltype))
                           " / 位置合わせ 中央(MC)"
                           " / 基準 " (if (= (bno:get 'ref) "C") "ブロック中心" "挿入点")
                           " / ずらし " (rtos (bno:get 'ox) 2 2) "," (rtos (bno:get 'oy) 2 2) "]"))))
  (princ (strcat "\n前回の最終番号: " (itoa (bno:get 'last))))
  (princ))

(defun bno:settings ( / v perp)
  ;; 番号の付け方
  (initget 4)
  (setq v (getint (strcat "\n番号の付け方 [1=全て別番号 / 2=同名ブロックは同じ番号 / 3=全て同じ番号] <"
                          (itoa (bno:get 'mode)) ">: ")))
  (if (and v (<= 1 v 3)) (bno:set 'mode v))
  ;; 並び順 (主方向)
  (initget "D U R L")
  (setq v (getkword (strcat "\n同じ行の中で進む向き [上から下(D)/下から上(U)/左から右(R)/右から左(L)] <"
                            (bno:dname (bno:get 'pdir)) ">: ")))
  (if v (bno:set 'pdir v))
  ;; 並び順 (副方向)
  (if (member (bno:get 'pdir) '("R" "L"))
    (progn
      (if (not (member (bno:get 'sdir) '("D" "U"))) (bno:set 'sdir "D"))
      (initget "D U")
      (setq v (getkword (strcat "\n次の行へ移る向き [上から下(D)/下から上(U)] <"
                                (bno:dname (bno:get 'sdir)) ">: "))))
    (progn
      (if (not (member (bno:get 'sdir) '("R" "L"))) (bno:set 'sdir "R"))
      (initget "R L")
      (setq v (getkword (strcat "\n次の列へ移る向き [左から右(R)/右から左(L)] <"
                                (bno:dname (bno:get 'sdir)) ">: ")))))
  (if v (bno:set 'sdir v))
  (bno:askreal "同じ行(列)とみなす幅(許容差)" 'tol 4)
  ;; 書式
  (bno:askint "増分" 'step 6)
  (bno:askstr "接頭文字" 'prefix)
  (bno:askstr "接尾文字" 'suffix)
  (bno:askint "桁数(ゼロ埋め。0=なし)" 'digits 4)
  ;; 出力先
  (initget "A T")
  (setq v (getkword (strcat "\n出力先 [ブロック属性に書込(A)/文字を新規作成(T)] <"
                            (if (= (bno:get 'out) "A") "A" "T") ">: ")))
  (if v (bno:set 'out v))
  (if (= (bno:get 'out) "A")
    (bno:askstr "属性タグ名" 'tag)
    (progn
      (bno:askstr "文字の画層" 'layer)
      (bno:askstr "文字スタイル" 'style)
      (bno:askreal "文字高さ" 'th 6)
      (bno:askreal "幅係数" 'wf 6)
      (bno:askstr "線種(なし=ByLayer)" 'ltype)
      (bno:askreal "線種尺度" 'lts 6)
      (initget "C I")
      (setq v (getkword (strcat "\n文字を置く基準 [ブロック中心(C)/挿入点(I)] <"
                                (bno:get 'ref) ">: ")))
      (if v (bno:set 'ref v))
      (bno:askreal "基準からのずらしX" 'ox 0)
      (bno:askreal "基準からのずらしY" 'oy 0)))
  (bno:save)
  (princ))

;;; ---------- メインコマンド ----------
(defun c:BNO ( / *error* ss ans loop i en pt lst ord asg start def nums old cnt miss uopen)
  (defun *error* (m)
    (if uopen (command "_.undo" "_end"))
    (if (and m (not (wcmatch (strcase m) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nエラー: " m)))
    (princ))
  (bno:load)
  (princ "\n番号を付けるブロックを窓選択してください")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss)
    (princ "\nブロックが選択されませんでした。")
    (progn
      ;; 設定確認ループ
      (setq loop T)
      (while loop
        (bno:show)
        (initget "S")
        (setq ans (getkword "\nオプション [設定変更(S)] <Enterで次へ>: "))
        (if (= ans "S") (bno:settings) (setq loop nil)))
      ;; 開始番号
      (setq def (+ (bno:get 'last) (bno:get 'step)))
      (initget 4)
      (setq start (getint (strcat "\n開始番号 <" (itoa def) ">: ")))
      (if (null start) (setq start def))
      ;; 並べ替え
      (setq i 0 lst nil)
      (repeat (sslength ss)
        (setq en (ssname ss i)
              pt (cdr (assoc 10 (entget en)))
              i (1+ i))
        (setq lst (cons (list en
                              (bno:coord pt (bno:get 'pdir))
                              (bno:coord pt (bno:get 'sdir)))
                        lst)))
      (setq ord (bno:order (reverse lst) (bno:get 'tol)))
      (setq asg (bno:assign (mapcar 'car ord) start))
      ;; 書き込み
      (setq cnt 0 miss 0)
      (command "_.undo" "_begin")
      (setq uopen T)
      (if (= (bno:get 'out) "T")
        (progn
          (if (not (tblsearch "APPID" "BNO")) (regapp "BNO"))
          (bno:prep)
          (setq old (bno:oldtexts))))
      (foreach a asg
        (if (= (bno:get 'out) "A")
          (if (bno:setattr (car a) (bno:get 'tag) (bno:fmt (cdr a)))
            (setq cnt (1+ cnt))
            (setq miss (1+ miss)))
          (if (bno:mktext (car a) (bno:fmt (cdr a)) old)
            (setq cnt (1+ cnt))
            (setq miss (1+ miss)))))
      (command "_.undo" "_end")
      (setq uopen nil)
      ;; 前回値の保存
      (setq nums (mapcar 'cdr asg))
      (bno:set 'last (apply 'max nums))
      (bno:save)
      (princ (strcat "\n" (itoa cnt) "個に番号を付けました ("
                     (bno:fmt (apply 'min nums)) " ～ " (bno:fmt (apply 'max nums)) ")。"))
      (if (> miss 0)
        (princ (strcat "\n※ " (itoa miss) "個は書き込めませんでした(属性タグ「"
                       (bno:get 'tag) "」が見つからない等)。")))))
  (princ))

(princ "\nBNO.lsp 読込完了 — コマンド: BNO")
(princ)