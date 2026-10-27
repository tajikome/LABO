;;; ============================================================
;;;  線種・太さ・長さ・面積・角度選択 (SQ)
;;;  旧 SLT / SLL / SPLL を SQ 1つに統合（コマンドは SQ のみ）
;;;
;;;  SQ  : 線種 / 太さ / 長さ / 面積 / 角度 の条件を白基調ダイアログで選んで一括選択
;;;        ダイアログ決定後に画面上で検索範囲を選択（Enter・右クリックで図面全体）
;;;        各リストは Shift+クリックで範囲選択 / 並び順は 標準・個数順
;;;        長さ・面積・角度は「基準と一致（許容誤差）」または「以上・以下の範囲」で指定
;;;        長さ = 線分・ポリライン・円(円周)・円弧・楕円・開いたスプライン
;;;        面積 = 円・楕円・閉じたスプライン・閉じたポリライン
;;;        長さ・面積の一覧には図形の種類（線分/ポリライン/円/円弧/楕円/スプライン）を表示し、種類も含めて一致判定
;;;        長さと面積を両方オンにすると「長さが一致 または 面積が一致」で判定（閉じたスプラインは面積、開いたスプラインは長さ）
;;;        各項目（線種・太さ・長さ・面積・角度）は左の矢印ボタンで折り畳み/展開できる
;;; ============================================================
(vl-load-com)

;; ---- レイヤーが操作可能か（ロック/フリーズ画層は除外）----
(defun slx-layer-ok (lay / r f)
  (if (setq r (assoc lay *slx-lc*))
    (cdr r)
    (progn
      (setq f (cdr (assoc 70 (tblsearch "LAYER" lay))))
      (setq f (if f (zerop (logand f 5)) T))
      (setq *slx-lc* (cons (cons lay f) *slx-lc*))
      f
    )
  )
)

;; ---- 線種プロパティ（DXF 6 が無ければ ByLayer）----
(defun slx-lt-of (ed / v)
  (if (setq v (assoc 6 ed)) (cdr v) "ByLayer")
)

;; ---- スプラインが閉じているか ----
(defun slx-spl-closed-p (ent / obj f)
  (setq obj (vlax-ename->vla-object ent))
  (setq f (vl-catch-all-apply 'vlax-curve-isClosed (list obj)))
  (if (vl-catch-all-error-p f)
    (setq f (and (vlax-property-available-p obj 'Closed)
                 (= (vla-get-closed obj) :vlax-true)))
  )
  (if f T nil)
)

;; ---- 曲線の長さ（円なら円周）----
(defun slx-curve-len (obj / r)
  (setq r (vl-catch-all-apply
            '(lambda ()
               (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
            nil))
  (if (and (numberp r) (> r 0.0)) r nil)
)

;; ---- 長さ取得（線分・ポリライン・円・円弧・楕円・開いたスプライン）----
(defun slx-len-of (ent ed / typ obj r)
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((member typ '("LINE" "LWPOLYLINE" "POLYLINE"))
     (setq obj (vlax-ename->vla-object ent))
     (if (vlax-property-available-p obj 'Length)
       (progn
         (setq r (vl-catch-all-apply 'vla-get-Length (list obj)))
         (if (vl-catch-all-error-p r) nil r)
       )
     )
    )
    ((member typ '("ARC" "CIRCLE" "ELLIPSE"))
     (slx-curve-len (vlax-ename->vla-object ent))
    )
    ((and (= typ "SPLINE") (not (slx-spl-closed-p ent)))
     (slx-curve-len (vlax-ename->vla-object ent))
    )
  )
)

;; ---- 面積取得（閉じたスプライン・円・楕円・閉じたポリライン）----
(defun slx-area-of (ent ed / obj r)
  (if (and (member (cdr (assoc 0 ed)) '("SPLINE" "CIRCLE" "ELLIPSE" "LWPOLYLINE" "POLYLINE"))
           (slx-spl-closed-p ent))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq r (vl-catch-all-apply 'vlax-curve-getArea (list obj)))
      (if (and (numberp r) (> r 0.0)) r nil)
    )
  )
)

;; ---- 角度（度）取得 ----
;; LINE=線の向き(0以上180未満) / INSERT・TEXT・MTEXT=回転角(0以上360未満)
;; 戻り値: (角度 . 周期) または nil
(defun slx-ang-of (ed / typ a per v)
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "LINE")
     (setq per 180.0
           a   (* (/ 180.0 pi) (angle (cdr (assoc 10 ed)) (cdr (assoc 11 ed))))))
    ((member typ '("INSERT" "TEXT"))
     (setq per 360.0
           a   (* (/ 180.0 pi) (if (setq v (assoc 50 ed)) (cdr v) 0.0))))
    ((= typ "MTEXT")
     (setq per 360.0
           a   (* (/ 180.0 pi)
                  (cond ((setq v (assoc 50 ed)) (cdr v))
                        ((setq v (assoc 11 ed)) (angle '(0.0 0.0 0.0) (cdr v)))
                        (T 0.0)))))
  )
  (if a
    (progn
      (setq a (- a (* per (fix (/ a per)))))
      (if (< a 0.0) (setq a (+ a per)))
      (setq a (atof (rtos a 2 2)))
      (if (>= a per) (setq a (- a per)))
      (cons a per)
    )
  )
)

;; ---- 集計用 ----
(defun slx-add-lt (key disp lst / r)
  (if (setq r (assoc key lst))
    (subst (list key (cadr r) (1+ (nth 2 r))) r lst)
    (append lst (list (list key disp 1)))
  )
)

(defun slx-add-len (key len lst / r)
  (if (setq r (assoc key lst))
    (subst (list key (cadr r) (1+ (nth 2 r))) r lst)
    (append lst (list (list key len 1)))
  )
)

;; ---- 図形の種類名（一覧表示・一致判定用）----
(defun slx-type-name (ed / typ)
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "LINE") "線分")
    ((member typ '("LWPOLYLINE" "POLYLINE")) "ポリライン")
    ((= typ "SPLINE") "スプライン")
    ((= typ "ARC") "円弧")
    ((= typ "CIRCLE") "円")
    ((= typ "ELLIPSE") "楕円")
    (T typ)
  )
)

;; 長さ・面積用: (KEY 値 件数 種類名)
(defun slx-add-lent (key val tn lst / r)
  (if (setq r (assoc key lst))
    (subst (list key (cadr r) (1+ (nth 2 r)) (nth 3 r)) r lst)
    (append lst (list (list key val 1 tn)))
  )
)

;; ---- 線の太さ（DXF 370: -1=ByLayer -2=ByBlock -3=既定 / 他は 1/100mm）----
(defun slx-lw-of (ed / v)
  (if (setq v (assoc 370 ed)) (cdr v) -1)
)

(defun slx-lw-name (v)
  (cond
    ((= v -1) "ByLayer")
    ((= v -2) "ByBlock")
    ((= v -3) "既定(Default)")
    (T (strcat (rtos (/ v 100.0) 2 2) " mm"))
  )
)

(defun slx-lw-rank (v)
  (if (< v 0) (+ -1000 (- v)) v)
)

;; 太さ: (KEY 表示名 件数 値)
(defun slx-add-lw (v lst / key r)
  (setq key (itoa v))
  (if (setq r (assoc key lst))
    (subst (list key (cadr r) (1+ (nth 2 r)) (nth 3 r)) r lst)
    (append lst (list (list key (slx-lw-name v) 1 v)))
  )
)

;; ---- 基準オブジェクトから条件候補を集計 ----
;; 戻り値: (有効な基準数  線種リスト  長さリスト  太さリスト  角度リスト  面積リスト)
;;   線種: (KEY 表示名 件数)   長さ・角度・面積: (KEY 値 件数)
(defun slx-collect (ss / i n ent ed lay lt len ltL lenL lwL an anL ar arL)
  (setq i 0 n 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          ed  (entget ent)
          i   (1+ i)
          lay (cdr (assoc 8 ed))
    )
    (if (slx-layer-ok lay)
      (progn
        (setq n (1+ n))
        (setq lt (slx-lt-of ed))
        (setq ltL (slx-add-lt (strcase lt) lt ltL))
        (setq lwL (slx-add-lw (slx-lw-of ed) lwL))
        (if (setq len (slx-len-of ent ed))
          (setq lenL (slx-add-lent (strcat (slx-type-name ed) ":" (rtos len 2 4)) len (slx-type-name ed) lenL))
        )
        (if (setq ar (slx-area-of ent ed))
          (setq arL (slx-add-lent (strcat (slx-type-name ed) ":" (rtos ar 2 4)) ar (slx-type-name ed) arL))
        )
        (if (setq an (slx-ang-of ed))
          (setq anL (slx-add-len (rtos (car an) 2 2) (car an) anL))
        )
      )
    )
  )
  (setq lenL (vl-sort lenL '(lambda (a b) (or (< (cadr a) (cadr b)) (and (= (cadr a) (cadr b)) (< (nth 3 a) (nth 3 b)))))))
  (setq arL  (vl-sort arL  '(lambda (a b) (or (< (cadr a) (cadr b)) (and (= (cadr a) (cadr b)) (< (nth 3 a) (nth 3 b)))))))
  (setq lwL (vl-sort lwL '(lambda (a b) (< (slx-lw-rank (nth 3 a)) (slx-lw-rank (nth 3 b))))))
  (setq anL (vl-sort anL '(lambda (a b) (< (cadr a) (cadr b)))))
  (list n ltL lenL lwL anL arL)
)

;; ---- ダイアログ用データ書き出し ----
(defun slx-write-data (path n ltL lenL lwL anL arL uLt uLen uLw uAn uAr
                       lmode lmin lmax srt amode amin amax armode armin armax / f tb)
  (setq tb (chr 9))
  (if (setq f (open path "w"))
    (progn
      (write-line
        (strcat "META" tb (itoa n) tb (rtos *slx-tol* 2 8) tb uLt tb uLen
                tb lmode tb lmin tb lmax tb uLw tb srt
                tb uAn tb amode tb amin tb amax tb (rtos *slx-atol* 2 8)
                tb uAr tb armode tb armin tb armax tb (rtos *slx-artol* 2 8))
        f
      )
      (foreach e ltL
        (write-line (strcat "LT" tb (cadr e) tb (itoa (nth 2 e))) f)
      )
      (foreach e lwL
        (write-line (strcat "LW" tb (cadr e) tb (itoa (nth 2 e))) f)
      )
      (foreach e lenL
        (write-line (strcat "LEN" tb (rtos (cadr e) 2 4) tb (itoa (nth 2 e)) tb (nth 3 e)) f)
      )
      (foreach e anL
        (write-line (strcat "AN" tb (rtos (cadr e) 2 2) tb (itoa (nth 2 e))) f)
      )
      (foreach e arL
        (write-line (strcat "AR" tb (rtos (cadr e) 2 4) tb (itoa (nth 2 e)) tb (nth 3 e)) f)
      )
      (close f)
      T
    )
  )
)

;; ---- ダイアログ結果の読み込み (KEY=VALUE) ----
(defun slx-read-res (path / f l p res)
  (if (and (findfile path) (setq f (open path "r")))
    (progn
      (while (setq l (read-line f))
        (if (setq p (vl-string-position (ascii "=") l))
          (setq res (cons (cons (substr l 1 p) (substr l (+ p 2))) res))
        )
      )
      (close f)
    )
  )
  res
)

(defun slx-split-ints (s / out p)
  (while (and s (/= s ""))
    (if (setq p (vl-string-search "," s))
      (setq out (cons (atoi (substr s 1 p)) out)
            s   (substr s (+ p 2))
      )
      (setq out (cons (atoi s) out)
            s   nil
      )
    )
  )
  (reverse out)
)

;; ---- PowerShell(WPF) ダイアログ ----
(defun slx-dialog (n ltL lenL lwL anL arL uLt uLen uLw uAn uAr / ps dat res sh cmd r)
  (setq ps  (vl-filename-mktemp "slx_ui" nil ".ps1")
        dat (vl-filename-mktemp "slx_dat" nil ".txt")
        res (vl-filename-mktemp "slx_res" nil ".txt")
  )
  (if (and (slx-write-ps ps)
           (slx-write-data dat n ltL lenL lwL anL arL uLt uLen uLw uAn uAr
                           *slx-lmode* *slx-lmin* *slx-lmax* *slx-sort*
                           *slx-amode* *slx-amin* *slx-amax*
                           *slx-armode* *slx-armin* *slx-armax*))
    (progn
      (setq sh (vlax-create-object "WScript.Shell"))
      (setq cmd
        (strcat "powershell.exe -NoProfile -ExecutionPolicy Bypass -STA -WindowStyle Hidden -File \""
                ps "\" \"" dat "\" \"" res "\"")
      )
      (vl-catch-all-apply 'vlax-invoke-method (list sh 'Run cmd 0 :vlax-true))
      (vlax-release-object sh)
      (setq r (slx-read-res res))
    )
  )
  (foreach f (list ps dat res)
    (if (findfile f) (vl-file-delete f))
  )
  r
)

(defun slx-write-ps (path / f)
  (if (setq f (open path "w"))
    (progn
      (foreach l (slx-ps-lines) (write-line l f))
      (close f)
      T
    )
  )
)

;; ---- 判定 ----
(defun slx-len-match (len tn sel tol / hit)
  (foreach e sel
    (if (and (not hit) (= (nth 3 e) tn) (<= (abs (- len (cadr e))) tol))
      (setq hit T)
    )
  )
  hit
)

(defun slx-num (s)
  (if (and s (/= s "")) (atof s))
)

;; 角度の判定: 一致(許容誤差・周期を考慮) / 範囲(以上・以下)
(defun slx-ang-test (ang q / a per amin amax tol hit d)
  (setq a (car ang) per (cdr ang))
  (if (= (cdr (assoc 'aMode q)) "RANGE")
    (setq amin (cdr (assoc 'aMin q))
          amax (cdr (assoc 'aMax q))
          hit  (and (or (null amin) (>= a (- amin 0.00000001)))
                    (or (null amax) (<= a (+ amax 0.00000001)))))
    (progn
      (setq tol (cdr (assoc 'aTol q)))
      (foreach e (cdr (assoc 'sAn q))
        (setq d (abs (- a (cadr e))))
        (setq d (- d (* per (fix (/ d per)))))
        (if (and (not hit) (<= (min d (- per d)) tol))
          (setq hit T)
        )
      )
    )
  )
  hit
)

;; 長さの判定: 基準と一致(許容誤差) / 範囲(以上・以下)
(defun slx-len-test (len tn q / lmin lmax)
  (if (= (cdr (assoc 'lMode q)) "RANGE")
    (progn
      (setq lmin (cdr (assoc 'lMin q))
            lmax (cdr (assoc 'lMax q)))
      (and (or (null lmin) (>= len (- lmin 0.00000001)))
           (or (null lmax) (<= len (+ lmax 0.00000001))))
    )
    (slx-len-match len tn (cdr (assoc 'sLen q)) (cdr (assoc 'tol q)))
  )
)

;; 面積の判定: 基準と一致(許容誤差) / 範囲(以上・以下)
(defun slx-area-test (ar tn q / amin amax)
  (if (= (cdr (assoc 'arMode q)) "RANGE")
    (progn
      (setq amin (cdr (assoc 'arMin q))
            amax (cdr (assoc 'arMax q)))
      (and (or (null amin) (>= ar (- amin 0.00000001)))
           (or (null amax) (<= ar (+ amax 0.00000001))))
    )
    (slx-len-match ar tn (cdr (assoc 'sAr q)) (cdr (assoc 'arTol q)))
  )
)

;; q: 条件の連想リスト (uLt sLt uLw sLw uLen lMode sLen tol lMin lMax uAr arMode sAr arTol arMin arMax
;;                      uAn aMode sAn aTol aMin aMax)
;; lMode/aMode/arMode: "MATCH"=基準と一致(許容誤差) / "RANGE"=以上・以下 (nilは制限なし)
;; 長さ・面積: 片方だけオン → その条件のみ判定 / 両方オン → 長さを持つ図形は長さ、面積を持つ図形は面積で判定
(defun slx-test (ent ed q / lay len ar ang)
  (setq lay (cdr (assoc 8 ed)))
  (and (slx-layer-ok lay)
       (or (not (cdr (assoc 'uLt q)))
           (member (strcase (slx-lt-of ed)) (cdr (assoc 'sLt q))))
       (or (not (cdr (assoc 'uLw q)))
           (member (itoa (slx-lw-of ed)) (cdr (assoc 'sLw q))))
       (cond
         ((and (cdr (assoc 'uLen q)) (cdr (assoc 'uAr q)))
          (or (and (setq len (slx-len-of ent ed))
                   (slx-len-test len (slx-type-name ed) q))
              (and (setq ar (slx-area-of ent ed))
                   (slx-area-test ar (slx-type-name ed) q))))
         ((cdr (assoc 'uLen q))
          (and (setq len (slx-len-of ent ed))
               (slx-len-test len (slx-type-name ed) q)))
         ((cdr (assoc 'uAr q))
          (and (setq ar (slx-area-of ent ed))
               (slx-area-test ar (slx-type-name ed) q)))
         (T T)
       )
       (or (not (cdr (assoc 'uAn q)))
           (and (setq ang (slx-ang-of ed))
                (slx-ang-test ang q)))
  )
)

;; ---- 検索対象の収集（画面上で範囲選択・Enter/右クリックで図面全体）----
(defun slx-get-target ( / ss)
  (princ "\n検索範囲を選択してください（Enter・右クリックで図面全体）: ")
  (setq ss (ssget))
  (if (null ss)
    (setq ss (ssget "_X" (list (cons 410 (getvar "CTAB")))))
  )
  ss
)

;; ---- ダイアログ結果 → 条件の連想リスト ----
(defun slx-parse-res (r ltL lenL lwL anL arL)
  (list
    (cons 'uLt   (= (cdr (assoc "USELT" r)) "1"))
    (cons 'sLt   (mapcar '(lambda (k) (car (nth k ltL)))
                         (slx-split-ints (cdr (assoc "LT" r)))))
    (cons 'uLw   (= (cdr (assoc "USELW" r)) "1"))
    (cons 'sLw   (mapcar '(lambda (k) (car (nth k lwL)))
                         (slx-split-ints (cdr (assoc "LW" r)))))
    (cons 'uLen  (= (cdr (assoc "USELEN" r)) "1"))
    (cons 'lMode (cdr (assoc "LMODE" r)))
    (cons 'sLen  (mapcar '(lambda (k) (nth k lenL))
                         (slx-split-ints (cdr (assoc "LEN" r)))))
    (cons 'tol   (atof (cdr (assoc "TOL" r))))
    (cons 'lMin  (slx-num (cdr (assoc "LMIN" r))))
    (cons 'lMax  (slx-num (cdr (assoc "LMAX" r))))
    (cons 'uAr   (= (cdr (assoc "USEAR" r)) "1"))
    (cons 'arMode (cdr (assoc "ARMODE" r)))
    (cons 'sAr   (mapcar '(lambda (k) (nth k arL))
                         (slx-split-ints (cdr (assoc "AR" r)))))
    (cons 'arTol (atof (cdr (assoc "ARTOL" r))))
    (cons 'arMin (slx-num (cdr (assoc "ARMIN" r))))
    (cons 'arMax (slx-num (cdr (assoc "ARMAX" r))))
    (cons 'uAn   (= (cdr (assoc "USEANG" r)) "1"))
    (cons 'aMode (cdr (assoc "AMODE" r)))
    (cons 'sAn   (mapcar '(lambda (k) (nth k anL))
                         (slx-split-ints (cdr (assoc "ANG" r)))))
    (cons 'aTol  (atof (cdr (assoc "ATOL" r))))
    (cons 'aMin  (slx-num (cdr (assoc "AMIN" r))))
    (cons 'aMax  (slx-num (cdr (assoc "AMAX" r))))
  )
)

;; ---- 次回の初期値として保持 ----
(defun slx-save-last (r q mode)
  (setq *slx-tol*   (cdr (assoc 'tol q))
        *slx-atol*  (cdr (assoc 'aTol q))
        *slx-artol* (cdr (assoc 'arTol q))
        *slx-sort*  (if (= (cdr (assoc "SORT" r)) "1") "1" "0")
        *slx-lmode* (cdr (assoc 'lMode q))
        *slx-lmin*  (if (cdr (assoc 'lMin q)) (cdr (assoc "LMIN" r)) "")
        *slx-lmax*  (if (cdr (assoc 'lMax q)) (cdr (assoc "LMAX" r)) "")
        *slx-armode* (cdr (assoc 'arMode q))
        *slx-armin* (if (cdr (assoc 'arMin q)) (cdr (assoc "ARMIN" r)) "")
        *slx-armax* (if (cdr (assoc 'arMax q)) (cdr (assoc "ARMAX" r)) "")
        *slx-amode* (cdr (assoc 'aMode q))
        *slx-amin*  (if (cdr (assoc 'aMin q)) (cdr (assoc "AMIN" r)) "")
        *slx-amax*  (if (cdr (assoc 'aMax q)) (cdr (assoc "AMAX" r)) "")
  )
  (if (= mode "SQ")
    (setq *slx-uselt*  (if (cdr (assoc 'uLt q)) "1" "0")
          *slx-uselw*  (if (cdr (assoc 'uLw q)) "1" "0")
          *slx-uselen* (if (cdr (assoc 'uLen q)) "1" "0")
          *slx-usear*  (if (cdr (assoc 'uAr q)) "1" "0")
          *slx-useang* (if (cdr (assoc 'uAn q)) "1" "0"))
  )
)

;; ---- メイン ----
(defun slx-main-body (mode / uLt uLen uLw uAn uAr refSs info n ltL lenL lwL anL arL r q ss out i ent ed)
  (setq *slx-lc* nil)
  (if (null *slx-tol*)    (setq *slx-tol* 0.001))
  (if (null *slx-atol*)   (setq *slx-atol* 0.01))
  (if (null *slx-artol*)  (setq *slx-artol* 0.001))
  (if (null *slx-sort*)   (setq *slx-sort* "0"))
  (if (null *slx-lmode*)  (setq *slx-lmode* "MATCH"))
  (if (null *slx-lmin*)   (setq *slx-lmin* ""))
  (if (null *slx-lmax*)   (setq *slx-lmax* ""))
  (if (null *slx-armode*) (setq *slx-armode* "MATCH"))
  (if (null *slx-armin*)  (setq *slx-armin* ""))
  (if (null *slx-armax*)  (setq *slx-armax* ""))
  (if (null *slx-amode*)  (setq *slx-amode* "MATCH"))
  (if (null *slx-amin*)   (setq *slx-amin* ""))
  (if (null *slx-amax*)   (setq *slx-amax* ""))
  (cond
    (T (setq uLt  (if *slx-uselt*  *slx-uselt*  "1")
             uLw  (if *slx-uselw*  *slx-uselw*  "0")
             uLen (if *slx-uselen* *slx-uselen* "1")
             uAr  (if *slx-usear*  *slx-usear*  "0")
             uAn  (if *slx-useang* *slx-useang* "0")))
  )
  (princ "\n基準となるオブジェクトを選択してください（複数可・Enterで確定）: ")
  (setq refSs (ssget))
  (cond
    ((null refSs)
     (princ "\n基準オブジェクトが選択されませんでした。"))
    (T
     (setq info (slx-collect refSs)
           n    (car info)
           ltL  (cadr info)
           lenL (caddr info)
           lwL  (nth 3 info)
           anL  (nth 4 info)
           arL  (nth 5 info))
     (if (= n 0)
       (princ "\n有効な基準オブジェクトがありません（ロック・フリーズ画層は除外されます）。")
       (progn
         (setq r (slx-dialog n ltL lenL lwL anL arL uLt uLen uLw uAn uAr))
         (cond
           ((null r)
            (princ "\nキャンセルされました。"))
           ((assoc "ERR" r)
            (princ (strcat "\nダイアログの起動に失敗しました: " (cdr (assoc "ERR" r)))))
           ((/= (cdr (assoc "OK" r)) "1")
            (princ "\nキャンセルされました。"))
           (T
            (setq q (slx-parse-res r ltL lenL lwL anL arL))
            (slx-save-last r q mode)
            (setq ss (slx-get-target))
            (if (null ss)
              (princ "\n検索対象のオブジェクトがありません。")
              (progn
                (setq out (ssadd) i 0)
                (repeat (sslength ss)
                  (setq ent (ssname ss i)
                        ed  (entget ent)
                        i   (1+ i))
                  (if (slx-test ent ed q)
                    (ssadd ent out)
                  )
                )
                (if (> (sslength out) 0)
                  (progn
                    (sssetfirst nil out)
                    (princ (strcat "\n条件に一致する " (itoa (sslength out))
                                   " 個のオブジェクトを選択しました。"))
                  )
                  (princ "\n条件に一致するオブジェクトは見つかりませんでした。")
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

;; 右クリック=Enter にするため、実行中だけ SHORTCUTMENU を 0 にして終了時に戻す
(defun slx-main (mode / sm r msg)
  (setq sm (vl-catch-all-apply 'getvar '("SHORTCUTMENU")))
  (if (not (numberp sm)) (setq sm nil))
  (if sm (vl-catch-all-apply 'setvar (list "SHORTCUTMENU" 0)))
  (setq r (vl-catch-all-apply 'slx-main-body (list mode)))
  (if sm (vl-catch-all-apply 'setvar (list "SHORTCUTMENU" sm)))
  (if (vl-catch-all-error-p r)
    (progn
      (setq msg (vl-catch-all-error-message r))
      (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*キャンセル*"))
        (princ (strcat "\nエラー: " msg))
      )
    )
  )
  (princ)
)

(defun c:SQ  () (slx-main "SQ")  (princ))

;; ---- 埋め込みPowerShell(WPF)スクリプト ----
(defun slx-ps-lines ()
  '(
    "param([string]$DataPath, [string]$ResPath)"
    "$ErrorActionPreference = 'Stop'"
    "$enc = [System.Text.Encoding]::Default"
    "$inv = [System.Globalization.CultureInfo]::InvariantCulture"
    "try {"
    "Add-Type -AssemblyName PresentationFramework"
    "Add-Type -AssemblyName PresentationCore"
    "Add-Type -AssemblyName WindowsBase"
    "$meta = @{}"
    "$ltItems = New-Object System.Collections.ArrayList"
    "$lwItems = New-Object System.Collections.ArrayList"
    "$lenItems = New-Object System.Collections.ArrayList"
    "$anItems = New-Object System.Collections.ArrayList"
    "$arItems = New-Object System.Collections.ArrayList"
    "foreach ($ln in [System.IO.File]::ReadAllLines($DataPath, $enc)) {"
    "  $p = $ln.Split([char]9)"
    "  if ($p[0] -eq 'META') {"
    "    $meta['base'] = $p[1]; $meta['tol'] = $p[2]; $meta['uselt'] = $p[3]"
    "    $meta['uselen'] = $p[4]"
    "    $meta['lmode'] = $p[5]; $meta['lmin'] = $p[6]; $meta['lmax'] = $p[7]"
    "    $meta['uselw'] = $p[8]; $meta['sort'] = $p[9]"
    "    $meta['useang'] = $p[10]; $meta['amode'] = $p[11]"
    "    $meta['amin'] = $p[12]; $meta['amax'] = $p[13]; $meta['atol'] = $p[14]"
    "    $meta['usear'] = $p[15]; $meta['armode'] = $p[16]"
    "    $meta['armin'] = $p[17]; $meta['armax'] = $p[18]; $meta['artol'] = $p[19]"
    "  } elseif ($p[0] -eq 'LT') {"
    "    [void]$ltItems.Add(@($p[1], $p[2]))"
    "  } elseif ($p[0] -eq 'LW') {"
    "    [void]$lwItems.Add(@($p[1], $p[2]))"
    "  } elseif ($p[0] -eq 'LEN') {"
    "    [void]$lenItems.Add(@($p[1], $p[2], $p[3]))"
    "  } elseif ($p[0] -eq 'AN') {"
    "    [void]$anItems.Add(@($p[1], $p[2]))"
    "  } elseif ($p[0] -eq 'AR') {"
    "    [void]$arItems.Add(@($p[1], $p[2], $p[3]))"
    "  }"
    "}"
    "$xaml = @'"
    "<Window xmlns='http://schemas.microsoft.com/winfx/2006/xaml/presentation'"
    "        xmlns:x='http://schemas.microsoft.com/winfx/2006/xaml'"
    "        Title='線種・太さ・長さ・面積・角度選択' Width='1020' Height='720' MinWidth='760' MinHeight='440'"
    "        WindowStartupLocation='CenterScreen' Topmost='True' ShowInTaskbar='False'"
    "        WindowStyle='None' AllowsTransparency='True' Background='Transparent'"
    "        ResizeMode='CanResizeWithGrip'"
    "        FontFamily='Yu Gothic UI, Meiryo UI, Segoe UI' FontSize='12'>"
    "  <Window.Resources>"
    "    <Style x:Key='Btn' TargetType='Button'>"
    "      <Setter Property='Padding' Value='18,8'/>"
    "      <Setter Property='Cursor' Value='Hand'/>"
    "      <Setter Property='Background' Value='#FFFFFF'/>"
    "      <Setter Property='BorderBrush' Value='#D1D5DB'/>"
    "      <Setter Property='BorderThickness' Value='1'/>"
    "      <Setter Property='Foreground' Value='#111827'/>"
    "      <Setter Property='Template'>"
    "        <Setter.Value>"
    "          <ControlTemplate TargetType='Button'>"
    "            <Border x:Name='bd' CornerRadius='9' Padding='{TemplateBinding Padding}'"
    "                    Background='{TemplateBinding Background}'"
    "                    BorderBrush='{TemplateBinding BorderBrush}'"
    "                    BorderThickness='{TemplateBinding BorderThickness}'>"
    "              <ContentPresenter HorizontalAlignment='Center' VerticalAlignment='Center'/>"
    "            </Border>"
    "            <ControlTemplate.Triggers>"
    "              <Trigger Property='IsMouseOver' Value='True'>"
    "                <Setter TargetName='bd' Property='Opacity' Value='0.82'/>"
    "              </Trigger>"
    "              <Trigger Property='IsEnabled' Value='False'>"
    "                <Setter TargetName='bd' Property='Opacity' Value='0.4'/>"
    "              </Trigger>"
    "            </ControlTemplate.Triggers>"
    "          </ControlTemplate>"
    "        </Setter.Value>"
    "      </Setter>"
    "    </Style>"
    "    <Style x:Key='Mini' TargetType='Button' BasedOn='{StaticResource Btn}'>"
    "      <Setter Property='Padding' Value='10,3'/>"
    "      <Setter Property='FontSize' Value='11'/>"
    "      <Setter Property='Margin' Value='6,0,0,0'/>"
    "      <Setter Property='Background' Value='#FFFFFF'/>"
    "    </Style>"
    "    <Style x:Key='Chev' TargetType='Button' BasedOn='{StaticResource Btn}'>"
    "      <Setter Property='Padding' Value='8,7'/>"
    "      <Setter Property='Margin' Value='0,0,10,0'/>"
    "      <Setter Property='BorderThickness' Value='0'/>"
    "      <Setter Property='Background' Value='#F3F4F6'/>"
    "    </Style>"
    "    <Style x:Key='Card' TargetType='Border'>"
    "      <Setter Property='Background' Value='#FFFFFF'/>"
    "      <Setter Property='BorderBrush' Value='#E5E7EB'/>"
    "      <Setter Property='BorderThickness' Value='1'/>"
    "      <Setter Property='CornerRadius' Value='12'/>"
    "      <Setter Property='Padding' Value='16,14,16,14'/>"
    "      <Setter Property='Margin' Value='0,0,0,12'/>"
    "      <Setter Property='Effect'>"
    "        <Setter.Value>"
    "          <DropShadowEffect BlurRadius='10' ShadowDepth='1' Opacity='0.07'/>"
    "        </Setter.Value>"
    "      </Setter>"
    "    </Style>"
    "    <Style x:Key='Switch' TargetType='CheckBox'>"
    "      <Setter Property='Cursor' Value='Hand'/>"
    "      <Setter Property='FontSize' Value='13'/>"
    "      <Setter Property='FontWeight' Value='SemiBold'/>"
    "      <Setter Property='Foreground' Value='#111827'/>"
    "      <Setter Property='Template'>"
    "        <Setter.Value>"
    "          <ControlTemplate TargetType='CheckBox'>"
    "            <StackPanel Orientation='Horizontal' Background='Transparent'>"
    "              <Grid Width='38' Height='22' Margin='0,0,10,0'>"
    "                <Border x:Name='track' CornerRadius='11' Background='#D1D5DB'/>"
    "                <Ellipse x:Name='thumb' Width='16' Height='16' Fill='#FFFFFF'"
    "                         HorizontalAlignment='Left' Margin='3,0,0,0'/>"
    "              </Grid>"
    "              <ContentPresenter VerticalAlignment='Center'/>"
    "            </StackPanel>"
    "            <ControlTemplate.Triggers>"
    "              <Trigger Property='IsChecked' Value='True'>"
    "                <Setter TargetName='track' Property='Background' Value='#2563EB'/>"
    "                <Setter TargetName='thumb' Property='HorizontalAlignment' Value='Right'/>"
    "                <Setter TargetName='thumb' Property='Margin' Value='0,0,3,0'/>"
    "              </Trigger>"
    "              <Trigger Property='IsEnabled' Value='False'>"
    "                <Setter Property='Opacity' Value='0.45'/>"
    "              </Trigger>"
    "            </ControlTemplate.Triggers>"
    "          </ControlTemplate>"
    "        </Setter.Value>"
    "      </Setter>"
    "    </Style>"
    "    <Style x:Key='Item' TargetType='CheckBox'>"
    "      <Setter Property='Cursor' Value='Hand'/>"
    "      <Setter Property='Foreground' Value='#111827'/>"
    "      <Setter Property='Template'>"
    "        <Setter.Value>"
    "          <ControlTemplate TargetType='CheckBox'>"
    "            <Border x:Name='row' CornerRadius='7' Padding='8,6' Background='Transparent'>"
    "              <StackPanel Orientation='Horizontal'>"
    "                <Border x:Name='box' Width='17' Height='17' CornerRadius='5'"
    "                        BorderThickness='1.5' BorderBrush='#9CA3AF'"
    "                        Background='#FFFFFF' Margin='0,0,10,0'>"
    "                  <Path x:Name='tick' Data='M 2.5,6.5 L 5.5,9.5 L 10.5,3.5'"
    "                        Stroke='#FFFFFF' StrokeThickness='2' Visibility='Collapsed'"
    "                        StrokeStartLineCap='Round' StrokeEndLineCap='Round'"
    "                        StrokeLineJoin='Round'/>"
    "                </Border>"
    "                <ContentPresenter VerticalAlignment='Center'/>"
    "              </StackPanel>"
    "            </Border>"
    "            <ControlTemplate.Triggers>"
    "              <Trigger Property='IsMouseOver' Value='True'>"
    "                <Setter TargetName='row' Property='Background' Value='#EFF6FF'/>"
    "              </Trigger>"
    "              <Trigger Property='IsChecked' Value='True'>"
    "                <Setter TargetName='box' Property='Background' Value='#2563EB'/>"
    "                <Setter TargetName='box' Property='BorderBrush' Value='#2563EB'/>"
    "                <Setter TargetName='tick' Property='Visibility' Value='Visible'/>"
    "              </Trigger>"
    "            </ControlTemplate.Triggers>"
    "          </ControlTemplate>"
    "        </Setter.Value>"
    "      </Setter>"
    "    </Style>"
    "    <Style x:Key='Seg' TargetType='RadioButton'>"
    "      <Setter Property='Cursor' Value='Hand'/>"
    "      <Setter Property='Foreground' Value='#374151'/>"
    "      <Setter Property='Template'>"
    "        <Setter.Value>"
    "          <ControlTemplate TargetType='RadioButton'>"
    "            <Border x:Name='sb' CornerRadius='9' Padding='14,6' Margin='0,0,8,0'"
    "                    Background='#FFFFFF' BorderBrush='#D1D5DB' BorderThickness='1'>"
    "              <ContentPresenter HorizontalAlignment='Center'/>"
    "            </Border>"
    "            <ControlTemplate.Triggers>"
    "              <Trigger Property='IsMouseOver' Value='True'>"
    "                <Setter TargetName='sb' Property='Background' Value='#F3F4F6'/>"
    "              </Trigger>"
    "              <Trigger Property='IsChecked' Value='True'>"
    "                <Setter TargetName='sb' Property='Background' Value='#EFF6FF'/>"
    "                <Setter TargetName='sb' Property='BorderBrush' Value='#2563EB'/>"
    "                <Setter Property='Foreground' Value='#1D4ED8'/>"
    "              </Trigger>"
    "              <Trigger Property='IsEnabled' Value='False'>"
    "                <Setter Property='Opacity' Value='0.45'/>"
    "              </Trigger>"
    "            </ControlTemplate.Triggers>"
    "          </ControlTemplate>"
    "        </Setter.Value>"
    "      </Setter>"
    "    </Style>"
    "  </Window.Resources>"
    "  <Border Margin='12' CornerRadius='14' Background='#FFFFFF'"
    "          BorderBrush='#E5E7EB' BorderThickness='1'>"
    "    <Border.Effect>"
    "      <DropShadowEffect BlurRadius='20' ShadowDepth='2' Opacity='0.25'/>"
    "    </Border.Effect>"
    "    <Grid>"
    "      <Grid.RowDefinitions>"
    "        <RowDefinition Height='Auto'/>"
    "        <RowDefinition Height='*'/>"
    "        <RowDefinition Height='Auto'/>"
    "      </Grid.RowDefinitions>"
    "      <Grid x:Name='hdr' Grid.Row='0' Margin='24,18,16,10' Background='Transparent'>"
    "        <Grid.ColumnDefinitions>"
    "          <ColumnDefinition Width='*'/>"
    "          <ColumnDefinition Width='Auto'/>"
    "        </Grid.ColumnDefinitions>"
    "        <StackPanel>"
    "          <TextBlock Text='線種・太さ・長さ・面積・角度選択' FontSize='18' FontWeight='SemiBold'"
    "                     Foreground='#111827'/>"
    "          <TextBlock x:Name='txtSub' Foreground='#6B7280' Margin='0,4,0,0'/>"
    "        </StackPanel>"
    "        <Button x:Name='btnX' Grid.Column='1' Content='×' FontSize='16'"
    "                Style='{StaticResource Mini}' Padding='11,1' VerticalAlignment='Top'/>"
    "      </Grid>"
    "      <ScrollViewer Grid.Row='1' VerticalScrollBarVisibility='Auto'>"
    "        <StackPanel Margin='24,6,24,4'>"
    "          <Border Background='#EFF6FF' CornerRadius='10' Padding='12,8' Margin='0,0,0,12'>"
    "            <TextBlock Foreground='#1D4ED8' TextWrapping='Wrap'"
    "                       Text='「選択」のあと、画面上で検索範囲を選択します（Enter・右クリックで図面全体）'/>"
    "          </Border>"
    "          <StackPanel Orientation='Horizontal' Margin='2,0,0,12'>"
    "            <TextBlock Text='並び順' VerticalAlignment='Center' Foreground='#6B7280'"
    "                       Margin='0,0,12,0'/>"
    "            <RadioButton x:Name='rbSortStd' Content='標準' GroupName='sortmode'"
    "                         Style='{StaticResource Seg}'/>"
    "            <RadioButton x:Name='rbSortCnt' Content='個数が多い順' GroupName='sortmode'"
    "                         Style='{StaticResource Seg}'/>"
    "          </StackPanel>"
    "          <Grid>"
    "            <Grid.ColumnDefinitions>"
    "              <ColumnDefinition Width='*'/>"
    "              <ColumnDefinition Width='*'/>"
    "            </Grid.ColumnDefinitions>"
    "            <StackPanel Grid.Column='0' Margin='0,0,7,0'>"
    "          <Border Style='{StaticResource Card}'>"
    "            <StackPanel>"
    "              <Grid>"
    "                <Grid.ColumnDefinitions>"
    "                  <ColumnDefinition Width='Auto'/>"
    "                  <ColumnDefinition Width='*'/>"
    "                </Grid.ColumnDefinitions>"
    "                <Button x:Name='tgLt' Grid.Column='0' Style='{StaticResource Chev}'"
    "                        VerticalAlignment='Center' ToolTip='折り畳み / 展開'>"
    "                  <Path x:Name='arLt' Data='M 1,2 L 6,7 L 11,2' Stroke='#4B5563' StrokeThickness='2'"
    "                        StrokeStartLineCap='Round' StrokeEndLineCap='Round' StrokeLineJoin='Round'"
    "                        Width='12' Height='9' Stretch='None' RenderTransformOrigin='0.5,0.5'/>"
    "                </Button>"
    "                <CheckBox x:Name='chkLt' Grid.Column='1' Content='線種で絞り込む'"
    "                          Style='{StaticResource Switch}' VerticalAlignment='Center'/>"
    "                <StackPanel Grid.Column='1' Orientation='Horizontal' HorizontalAlignment='Right'>"
    "                  <Button x:Name='ltAll' Content='全選択' Style='{StaticResource Mini}'/>"
    "                  <Button x:Name='ltNone' Content='全解除' Style='{StaticResource Mini}'/>"
    "                </StackPanel>"
    "              </Grid>"
    "              <StackPanel x:Name='bdLt'>"
    "                <StackPanel x:Name='pnlLt' Margin='0,10,0,0'/>"
    "              </StackPanel>"
    "            </StackPanel>"
    "          </Border>"
    "          <Border Style='{StaticResource Card}'>"
    "            <StackPanel>"
    "              <Grid>"
    "                <Grid.ColumnDefinitions>"
    "                  <ColumnDefinition Width='Auto'/>"
    "                  <ColumnDefinition Width='*'/>"
    "                </Grid.ColumnDefinitions>"
    "                <Button x:Name='tgLw' Grid.Column='0' Style='{StaticResource Chev}'"
    "                        VerticalAlignment='Center' ToolTip='折り畳み / 展開'>"
    "                  <Path x:Name='arLw' Data='M 1,2 L 6,7 L 11,2' Stroke='#4B5563' StrokeThickness='2'"
    "                        StrokeStartLineCap='Round' StrokeEndLineCap='Round' StrokeLineJoin='Round'"
    "                        Width='12' Height='9' Stretch='None' RenderTransformOrigin='0.5,0.5'/>"
    "                </Button>"
    "                <CheckBox x:Name='chkLw' Grid.Column='1' Content='太さ（線の太さ）で絞り込む'"
    "                          Style='{StaticResource Switch}' VerticalAlignment='Center'/>"
    "                <StackPanel Grid.Column='1' Orientation='Horizontal' HorizontalAlignment='Right'>"
    "                  <Button x:Name='lwAll' Content='全選択' Style='{StaticResource Mini}'/>"
    "                  <Button x:Name='lwNone' Content='全解除' Style='{StaticResource Mini}'/>"
    "                </StackPanel>"
    "              </Grid>"
    "              <StackPanel x:Name='bdLw'>"
    "                <StackPanel x:Name='pnlLw' Margin='0,10,0,0'/>"
    "              </StackPanel>"
    "            </StackPanel>"
    "          </Border>"
    "          <Border Style='{StaticResource Card}'>"
    "            <StackPanel>"
    "              <Grid>"
    "                <Grid.ColumnDefinitions>"
    "                  <ColumnDefinition Width='Auto'/>"
    "                  <ColumnDefinition Width='*'/>"
    "                </Grid.ColumnDefinitions>"
    "                <Button x:Name='tgAng' Grid.Column='0' Style='{StaticResource Chev}'"
    "                        VerticalAlignment='Center' ToolTip='折り畳み / 展開'>"
    "                  <Path x:Name='arAng' Data='M 1,2 L 6,7 L 11,2' Stroke='#4B5563' StrokeThickness='2'"
    "                        StrokeStartLineCap='Round' StrokeEndLineCap='Round' StrokeLineJoin='Round'"
    "                        Width='12' Height='9' Stretch='None' RenderTransformOrigin='0.5,0.5'/>"
    "                </Button>"
    "                <CheckBox x:Name='chkAng' Grid.Column='1' Content='角度で絞り込む'"
    "                          Style='{StaticResource Switch}' VerticalAlignment='Center'/>"
    "                <StackPanel Grid.Column='1' Orientation='Horizontal' HorizontalAlignment='Right'>"
    "                  <Button x:Name='angAll' Content='全選択' Style='{StaticResource Mini}'/>"
    "                  <Button x:Name='angNone' Content='全解除' Style='{StaticResource Mini}'/>"
    "                </StackPanel>"
    "              </Grid>"
    "              <StackPanel x:Name='bdAng'>"
    "              <StackPanel Orientation='Horizontal' Margin='0,12,0,0'>"
    "                <RadioButton x:Name='rbAngMatch' Content='基準の角度と一致'"
    "                             GroupName='angmode' Style='{StaticResource Seg}'/>"
    "                <RadioButton x:Name='rbAngRange' Content='範囲で指定（以上・以下）'"
    "                             GroupName='angmode' Style='{StaticResource Seg}'/>"
    "              </StackPanel>"
    "              <StackPanel x:Name='pnlAMatch'>"
    "                <StackPanel x:Name='pnlAng' Margin='0,8,0,0'/>"
    "                <StackPanel Orientation='Horizontal' Margin='0,12,0,0'>"
    "                  <TextBlock Text='許容誤差' VerticalAlignment='Center' Foreground='#374151'/>"
    "                  <TextBox x:Name='txtATol' Width='90' Margin='8,0,0,0' Padding='5,4'"
    "                           BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='°' VerticalAlignment='Center' Margin='4,0,0,0'/>"
    "                </StackPanel>"
    "              </StackPanel>"
    "              <StackPanel x:Name='pnlARange' Margin='0,14,0,0'>"
    "                <StackPanel Orientation='Horizontal'>"
    "                  <TextBox x:Name='txtAMin' Width='100' Padding='5,4' BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='° 以上' VerticalAlignment='Center' Margin='6,0,24,0'/>"
    "                  <TextBox x:Name='txtAMax' Width='100' Padding='5,4' BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='° 以下' VerticalAlignment='Center' Margin='6,0,0,0'/>"
    "                </StackPanel>"
    "                <TextBlock Text='線は向き（0〜180°）、ブロック・文字は回転角（0〜360°）。空欄は制限なし'"
    "                           Foreground='#6B7280' FontSize='11' Margin='0,8,0,0'/>"
    "              </StackPanel>"
    "              </StackPanel>"
    "            </StackPanel>"
    "          </Border>"
    "            </StackPanel>"
    "            <StackPanel Grid.Column='1' Margin='7,0,0,0'>"
    "          <Border Style='{StaticResource Card}'>"
    "            <StackPanel>"
    "              <Grid>"
    "                <Grid.ColumnDefinitions>"
    "                  <ColumnDefinition Width='Auto'/>"
    "                  <ColumnDefinition Width='*'/>"
    "                </Grid.ColumnDefinitions>"
    "                <Button x:Name='tgLen' Grid.Column='0' Style='{StaticResource Chev}'"
    "                        VerticalAlignment='Center' ToolTip='折り畳み / 展開'>"
    "                  <Path x:Name='arLen' Data='M 1,2 L 6,7 L 11,2' Stroke='#4B5563' StrokeThickness='2'"
    "                        StrokeStartLineCap='Round' StrokeEndLineCap='Round' StrokeLineJoin='Round'"
    "                        Width='12' Height='9' Stretch='None' RenderTransformOrigin='0.5,0.5'/>"
    "                </Button>"
    "                <CheckBox x:Name='chkLen' Grid.Column='1' Content='長さで絞り込む' ToolTip='線分・ポリライン・円・円弧・楕円・開いたスプライン'"
    "                          Style='{StaticResource Switch}' VerticalAlignment='Center'/>"
    "                <StackPanel Grid.Column='1' Orientation='Horizontal' HorizontalAlignment='Right'>"
    "                  <Button x:Name='lenAll' Content='全選択' Style='{StaticResource Mini}'/>"
    "                  <Button x:Name='lenNone' Content='全解除' Style='{StaticResource Mini}'/>"
    "                </StackPanel>"
    "              </Grid>"
    "              <StackPanel x:Name='bdLen'>"
    "              <StackPanel x:Name='pnlLenMode' Orientation='Horizontal' Margin='0,12,0,0'>"
    "                <RadioButton x:Name='rbLenMatch' Content='基準の長さと一致'"
    "                             GroupName='lenmode' Style='{StaticResource Seg}'/>"
    "                <RadioButton x:Name='rbLenRange' Content='範囲で指定（以上・以下）'"
    "                             GroupName='lenmode' Style='{StaticResource Seg}'/>"
    "              </StackPanel>"
    "              <StackPanel x:Name='pnlMatch'>"
    "                <StackPanel x:Name='pnlLen' Margin='0,8,0,0'/>"
    "                <StackPanel Orientation='Horizontal' Margin='0,12,0,0'>"
    "                  <TextBlock Text='許容誤差' VerticalAlignment='Center' Foreground='#374151'/>"
    "                  <TextBox x:Name='txtTol' Width='90' Margin='8,0,0,0' Padding='5,4'"
    "                           BorderBrush='#D1D5DB'/>"
    "                </StackPanel>"
    "              </StackPanel>"
    "              <StackPanel x:Name='pnlRange' Margin='0,14,0,0'>"
    "                <StackPanel Orientation='Horizontal'>"
    "                  <TextBox x:Name='txtMin' Width='100' Padding='5,4' BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='mm 以上' VerticalAlignment='Center' Margin='6,0,24,0'/>"
    "                  <TextBox x:Name='txtMax' Width='100' Padding='5,4' BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='mm 以下' VerticalAlignment='Center' Margin='6,0,0,0'/>"
    "                </StackPanel>"
    "                <TextBlock Text='空欄は制限なし（例: 以下だけ入力 = ○mm以下）'"
    "                           Foreground='#6B7280' FontSize='11' Margin='0,8,0,0'/>"
    "              </StackPanel>"
    "              </StackPanel>"
    "            </StackPanel>"
    "          </Border>"
    "          <Border Style='{StaticResource Card}'>"
    "            <StackPanel>"
    "              <Grid>"
    "                <Grid.ColumnDefinitions>"
    "                  <ColumnDefinition Width='Auto'/>"
    "                  <ColumnDefinition Width='*'/>"
    "                </Grid.ColumnDefinitions>"
    "                <Button x:Name='tgArea' Grid.Column='0' Style='{StaticResource Chev}'"
    "                        VerticalAlignment='Center' ToolTip='折り畳み / 展開'>"
    "                  <Path x:Name='arArea' Data='M 1,2 L 6,7 L 11,2' Stroke='#4B5563' StrokeThickness='2'"
    "                        StrokeStartLineCap='Round' StrokeEndLineCap='Round' StrokeLineJoin='Round'"
    "                        Width='12' Height='9' Stretch='None' RenderTransformOrigin='0.5,0.5'/>"
    "                </Button>"
    "                <CheckBox x:Name='chkArea' Grid.Column='1' Content='面積で絞り込む' ToolTip='円・楕円・閉じたスプライン・閉じたポリライン'"
    "                          Style='{StaticResource Switch}' VerticalAlignment='Center'/>"
    "                <StackPanel Grid.Column='1' Orientation='Horizontal' HorizontalAlignment='Right'>"
    "                  <Button x:Name='areaAll' Content='全選択' Style='{StaticResource Mini}'/>"
    "                  <Button x:Name='areaNone' Content='全解除' Style='{StaticResource Mini}'/>"
    "                </StackPanel>"
    "              </Grid>"
    "              <StackPanel x:Name='bdArea'>"
    "              <StackPanel x:Name='pnlAreaMode' Orientation='Horizontal' Margin='0,12,0,0'>"
    "                <RadioButton x:Name='rbAreaMatch' Content='基準の面積と一致'"
    "                             GroupName='areamode' Style='{StaticResource Seg}'/>"
    "                <RadioButton x:Name='rbAreaRange' Content='範囲で指定（以上・以下）'"
    "                             GroupName='areamode' Style='{StaticResource Seg}'/>"
    "              </StackPanel>"
    "              <StackPanel x:Name='pnlAreaMatch'>"
    "                <StackPanel x:Name='pnlArea' Margin='0,8,0,0'/>"
    "                <StackPanel Orientation='Horizontal' Margin='0,12,0,0'>"
    "                  <TextBlock Text='許容誤差' VerticalAlignment='Center' Foreground='#374151'/>"
    "                  <TextBox x:Name='txtAreaTol' Width='90' Margin='8,0,0,0' Padding='5,4'"
    "                           BorderBrush='#D1D5DB'/>"
    "                </StackPanel>"
    "              </StackPanel>"
    "              <StackPanel x:Name='pnlAreaRange' Margin='0,14,0,0'>"
    "                <StackPanel Orientation='Horizontal'>"
    "                  <TextBox x:Name='txtAreaMin' Width='100' Padding='5,4' BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='mm2 以上' VerticalAlignment='Center' Margin='6,0,24,0'/>"
    "                  <TextBox x:Name='txtAreaMax' Width='100' Padding='5,4' BorderBrush='#D1D5DB'/>"
    "                  <TextBlock Text='mm2 以下' VerticalAlignment='Center' Margin='6,0,0,0'/>"
    "                </StackPanel>"
    "                <TextBlock Text='閉じた図形の面積。空欄は制限なし（例: 以下だけ入力 = ○mm2以下）'"
    "                           Foreground='#6B7280' FontSize='11' Margin='0,8,0,0'/>"
    "              </StackPanel>"
    "              </StackPanel>"
    "            </StackPanel>"
    "          </Border>"
    "            </StackPanel>"
    "          </Grid>"
    "        </StackPanel>"
    "      </ScrollViewer>"
    "      <Grid Grid.Row='2' Margin='24,8,24,20'>"
    "        <Grid.ColumnDefinitions>"
    "          <ColumnDefinition Width='*'/>"
    "          <ColumnDefinition Width='Auto'/>"
    "        </Grid.ColumnDefinitions>"
    "        <TextBlock x:Name='txtMsg' Foreground='#DC2626' VerticalAlignment='Center'"
    "                   TextWrapping='Wrap' Margin='0,0,12,0'/>"
    "        <StackPanel Grid.Column='1' Orientation='Horizontal'>"
    "          <Button x:Name='btnCancel' Content='キャンセル' IsCancel='True'"
    "                  Style='{StaticResource Btn}' Margin='0,0,8,0'/>"
    "          <Button x:Name='btnOk' Content='選択' IsDefault='True' MinWidth='110'"
    "                  Style='{StaticResource Btn}' Background='#2563EB'"
    "                  BorderBrush='#2563EB' Foreground='#FFFFFF' FontWeight='SemiBold'/>"
    "        </StackPanel>"
    "      </Grid>"
    "    </Grid>"
    "  </Border>"
    "</Window>"
    "'@"
    "$win = [Windows.Markup.XamlReader]::Load((New-Object System.Xml.XmlNodeReader ([xml]$xaml)))"
    "$win.MaxHeight = [System.Windows.SystemParameters]::WorkArea.Height"
    "$names = 'hdr','txtSub','btnX','chkLt','ltAll','ltNone','pnlLt','chkLw','lwAll','lwNone'"
    "$names += 'pnlLw','chkLen','lenAll','lenNone','pnlLen','txtTol'"
    "$names += 'btnOk','btnCancel','txtMsg','rbLenMatch','rbLenRange','pnlLenMode'"
    "$names += 'pnlMatch','pnlRange','txtMin','txtMax','rbSortStd','rbSortCnt'"
    "$names += 'chkAng','angAll','angNone','pnlAng','rbAngMatch','rbAngRange'"
    "$names += 'pnlAMatch','pnlARange','txtATol','txtAMin','txtAMax'"
    "$names += 'tgLt','arLt','bdLt','tgLw','arLw','bdLw'"
    "$names += 'tgLen','arLen','bdLen','tgAng','arAng','bdAng'"
    "$names += 'chkArea','areaAll','areaNone','pnlArea','rbAreaMatch','rbAreaRange'"
    "$names += 'pnlAreaMode','pnlAreaMatch','pnlAreaRange','txtAreaTol','txtAreaMin','txtAreaMax'"
    "$names += 'tgArea','arArea','bdArea'"
    "$c = @{}"
    "foreach ($nm in $names) { $c[$nm] = $win.FindName($nm) }"
    "$bc = New-Object System.Windows.Media.BrushConverter"
    "$brGray = $bc.ConvertFromString('#6B7280')"
    "$brDark = $bc.ConvertFromString('#374151')"
    "$brChip = $bc.ConvertFromString('#E5E7EB')"
    "function New-Row($main, $sub, $count) {"
    "  $sp = New-Object System.Windows.Controls.StackPanel"
    "  $sp.Orientation = 'Horizontal'"
    "  $t1 = New-Object System.Windows.Controls.TextBlock"
    "  $t1.Text = $main"
    "  $t1.VerticalAlignment = 'Center'"
    "  [void]$sp.Children.Add($t1)"
    "  if ($sub -ne '') {"
    "    $t2 = New-Object System.Windows.Controls.TextBlock"
    "    $t2.Text = $sub"
    "    $t2.Foreground = $brGray"
    "    $t2.VerticalAlignment = 'Center'"
    "    $t2.Margin = New-Object System.Windows.Thickness(12, 0, 0, 0)"
    "    [void]$sp.Children.Add($t2)"
    "  }"
    "  $t3 = New-Object System.Windows.Controls.TextBlock"
    "  $t3.Text = $count"
    "  $t3.FontSize = 11"
    "  $t3.Foreground = $brDark"
    "  $bd = New-Object System.Windows.Controls.Border"
    "  $bd.Background = $brChip"
    "  $bd.CornerRadius = New-Object System.Windows.CornerRadius(9)"
    "  $bd.Padding = New-Object System.Windows.Thickness(8, 1, 8, 1)"
    "  $bd.Margin = New-Object System.Windows.Thickness(12, 0, 0, 0)"
    "  $bd.Child = $t3"
    "  [void]$sp.Children.Add($bd)"
    "  return ,$sp"
    "}"
    "function New-Check($content, $idx, $count) {"
    "  $cb = New-Object System.Windows.Controls.CheckBox"
    "  $cb.Style = $win.FindResource('Item')"
    "  $cb.Content = $content"
    "  $cb.IsChecked = $true"
    "  $cb.Tag = $idx"
    "  $cb.DataContext = $idx"
    "  $cb.Uid = [string]$count"
    "  return ,$cb"
    "}"
    "$last = @{ lt = -1; lw = -1; len = -1; ang = -1; area = -1 }"
    "function Range-Click($sender, $boxes, $key) {"
    "  $idx = [int]$sender.Tag"
    "  $mod = [System.Windows.Input.Keyboard]::Modifiers"
    "  $shift = (($mod -band [System.Windows.Input.ModifierKeys]::Shift) -ne 0)"
    "  $prev = $script:last[$key]"
    "  if ($shift -and ($prev -ge 0)) {"
    "    $a = [Math]::Min($idx, $prev)"
    "    $b = [Math]::Max($idx, $prev)"
    "    $state = [bool]$sender.IsChecked"
    "    for ($j = $a; $j -le $b; $j++) { $boxes[$j].IsChecked = $state }"
    "  }"
    "  $script:last[$key] = $idx"
    "}"
    "function Sort-List($boxes, $panel, $key, $byCount) {"
    "  $arr = @($boxes)"
    "  if ($byCount) {"
    "    $k1 = @{ Expression = { [int]$_.Uid }; Descending = $true }"
    "    $k2 = @{ Expression = { [int]$_.DataContext } }"
    "    $sorted = $arr | Sort-Object -Property $k1, $k2"
    "  } else {"
    "    $sorted = $arr | Sort-Object -Property @{ Expression = { [int]$_.DataContext } }"
    "  }"
    "  $panel.Children.Clear()"
    "  $boxes.Clear()"
    "  $pos = 0"
    "  foreach ($b in $sorted) {"
    "    $b.Tag = $pos"
    "    [void]$panel.Children.Add($b)"
    "    [void]$boxes.Add($b)"
    "    $pos++"
    "  }"
    "  $script:last[$key] = -1"
    "}"
    "function Apply-Sort {"
    "  $cnt = [bool]$c.rbSortCnt.IsChecked"
    "  Sort-List $ltBoxes $c.pnlLt 'lt' $cnt"
    "  Sort-List $lwBoxes $c.pnlLw 'lw' $cnt"
    "  Sort-List $lenBoxes $c.pnlLen 'len' $cnt"
    "  Sort-List $anBoxes $c.pnlAng 'ang' $cnt"
    "  Sort-List $arBoxes $c.pnlArea 'area' $cnt"
    "}"
    "$c.txtSub.Text = ('基準 ' + $meta['base'] + ' 個から条件を作成  /  Shift+クリックで範囲選択  /  右クリックで決定')"
    "$ltBoxes = New-Object System.Collections.ArrayList"
    "$i = 0"
    "foreach ($it in $ltItems) {"
    "  $cb = New-Check (New-Row $it[0] '' $it[1]) $i $it[1]"
    "  $cb.Add_Click({ param($s, $e) Range-Click $s $ltBoxes 'lt' })"
    "  [void]$c.pnlLt.Children.Add($cb)"
    "  [void]$ltBoxes.Add($cb)"
    "  $i++"
    "}"
    "$lwBoxes = New-Object System.Collections.ArrayList"
    "$i = 0"
    "foreach ($it in $lwItems) {"
    "  $cb = New-Check (New-Row $it[0] '' $it[1]) $i $it[1]"
    "  $cb.Add_Click({ param($s, $e) Range-Click $s $lwBoxes 'lw' })"
    "  [void]$c.pnlLw.Children.Add($cb)"
    "  [void]$lwBoxes.Add($cb)"
    "  $i++"
    "}"
    "$lenBoxes = New-Object System.Collections.ArrayList"
    "$i = 0"
    "foreach ($it in $lenItems) {"
    "  $cb = New-Check (New-Row $it[0] $it[2] $it[1]) $i $it[1]"
    "  $cb.Add_Click({ param($s, $e) Range-Click $s $lenBoxes 'len' })"
    "  [void]$c.pnlLen.Children.Add($cb)"
    "  [void]$lenBoxes.Add($cb)"
    "  $i++"
    "}"
    "$anBoxes = New-Object System.Collections.ArrayList"
    "$i = 0"
    "foreach ($it in $anItems) {"
    "  $cb = New-Check (New-Row ($it[0] + ' °') '' $it[1]) $i $it[1]"
    "  $cb.Add_Click({ param($s, $e) Range-Click $s $anBoxes 'ang' })"
    "  [void]$c.pnlAng.Children.Add($cb)"
    "  [void]$anBoxes.Add($cb)"
    "  $i++"
    "}"
    "$arBoxes = New-Object System.Collections.ArrayList"
    "$i = 0"
    "foreach ($it in $arItems) {"
    "  $cb = New-Check (New-Row $it[0] $it[2] $it[1]) $i $it[1]"
    "  $cb.Add_Click({ param($s, $e) Range-Click $s $arBoxes 'area' })"
    "  [void]$c.pnlArea.Children.Add($cb)"
    "  [void]$arBoxes.Add($cb)"
    "  $i++"
    "}"
    "$c.chkLt.IsChecked = ($meta['uselt'] -eq '1')"
    "$c.chkLw.IsChecked = ($meta['uselw'] -eq '1')"
    "$c.chkLen.IsChecked = ($meta['uselen'] -eq '1')"
    "if ($meta['lmode'] -eq 'RANGE') { $c.rbLenRange.IsChecked = $true } else { $c.rbLenMatch.IsChecked = $true }"
    "if ($lenBoxes.Count -eq 0) {"
    "  $c.rbLenMatch.IsEnabled = $false"
    "  $c.rbLenRange.IsChecked = $true"
    "}"
    "$c.txtMin.Text = $meta['lmin']"
    "$c.txtMax.Text = $meta['lmax']"
    "$c.txtTol.Text = $meta['tol']"
    "$c.chkAng.IsChecked = ($meta['useang'] -eq '1')"
    "if ($meta['amode'] -eq 'RANGE') { $c.rbAngRange.IsChecked = $true } else { $c.rbAngMatch.IsChecked = $true }"
    "if ($anBoxes.Count -eq 0) {"
    "  $c.rbAngMatch.IsEnabled = $false"
    "  $c.rbAngRange.IsChecked = $true"
    "}"
    "$c.txtAMin.Text = $meta['amin']"
    "$c.txtAMax.Text = $meta['amax']"
    "$c.txtATol.Text = $meta['atol']"
    "$c.chkArea.IsChecked = ($meta['usear'] -eq '1')"
    "if ($meta['armode'] -eq 'RANGE') { $c.rbAreaRange.IsChecked = $true } else { $c.rbAreaMatch.IsChecked = $true }"
    "if ($arBoxes.Count -eq 0) {"
    "  $c.rbAreaMatch.IsEnabled = $false"
    "  $c.rbAreaRange.IsChecked = $true"
    "}"
    "$c.txtAreaMin.Text = $meta['armin']"
    "$c.txtAreaMax.Text = $meta['armax']"
    "$c.txtAreaTol.Text = $meta['artol']"
    "if ($meta['sort'] -eq '1') { $c.rbSortCnt.IsChecked = $true } else { $c.rbSortStd.IsChecked = $true }"
    "Apply-Sort"
    "function Update-State {"
    "  $on = [bool]$c.chkLt.IsChecked"
    "  $c.pnlLt.IsEnabled = $on"
    "  $c.ltAll.IsEnabled = $on"
    "  $c.ltNone.IsEnabled = $on"
    "  $c.pnlLt.Opacity = $(if ($on) { 1.0 } else { 0.4 })"
    "  $onw = [bool]$c.chkLw.IsChecked"
    "  $c.pnlLw.IsEnabled = $onw"
    "  $c.lwAll.IsEnabled = $onw"
    "  $c.lwNone.IsEnabled = $onw"
    "  $c.pnlLw.Opacity = $(if ($onw) { 1.0 } else { 0.4 })"
    "  $on2 = [bool]$c.chkLen.IsChecked"
    "  $isRange = [bool]$c.rbLenRange.IsChecked"
    "  $c.rbLenMatch.IsEnabled = ($on2 -and ($lenBoxes.Count -gt 0))"
    "  $c.rbLenRange.IsEnabled = $on2"
    "  $c.pnlMatch.IsEnabled = $on2"
    "  $c.pnlRange.IsEnabled = $on2"
    "  $c.lenAll.IsEnabled = ($on2 -and (-not $isRange))"
    "  $c.lenNone.IsEnabled = ($on2 -and (-not $isRange))"
    "  $c.pnlMatch.Visibility = $(if ($isRange) { 'Collapsed' } else { 'Visible' })"
    "  $c.pnlRange.Visibility = $(if ($isRange) { 'Visible' } else { 'Collapsed' })"
    "  $c.pnlMatch.Opacity = $(if ($on2) { 1.0 } else { 0.4 })"
    "  $c.pnlRange.Opacity = $(if ($on2) { 1.0 } else { 0.4 })"
    "  $on3 = [bool]$c.chkAng.IsChecked"
    "  $isARange = [bool]$c.rbAngRange.IsChecked"
    "  $c.rbAngMatch.IsEnabled = ($on3 -and ($anBoxes.Count -gt 0))"
    "  $c.rbAngRange.IsEnabled = $on3"
    "  $c.pnlAMatch.IsEnabled = $on3"
    "  $c.pnlARange.IsEnabled = $on3"
    "  $c.angAll.IsEnabled = ($on3 -and (-not $isARange))"
    "  $c.angNone.IsEnabled = ($on3 -and (-not $isARange))"
    "  $c.pnlAMatch.Visibility = $(if ($isARange) { 'Collapsed' } else { 'Visible' })"
    "  $c.pnlARange.Visibility = $(if ($isARange) { 'Visible' } else { 'Collapsed' })"
    "  $c.pnlAMatch.Opacity = $(if ($on3) { 1.0 } else { 0.4 })"
    "  $c.pnlARange.Opacity = $(if ($on3) { 1.0 } else { 0.4 })"
    "  $on4 = [bool]$c.chkArea.IsChecked"
    "  $isAreaRange = [bool]$c.rbAreaRange.IsChecked"
    "  $c.rbAreaMatch.IsEnabled = ($on4 -and ($arBoxes.Count -gt 0))"
    "  $c.rbAreaRange.IsEnabled = $on4"
    "  $c.pnlAreaMatch.IsEnabled = $on4"
    "  $c.pnlAreaRange.IsEnabled = $on4"
    "  $c.areaAll.IsEnabled = ($on4 -and (-not $isAreaRange))"
    "  $c.areaNone.IsEnabled = ($on4 -and (-not $isAreaRange))"
    "  $c.pnlAreaMatch.Visibility = $(if ($isAreaRange) { 'Collapsed' } else { 'Visible' })"
    "  $c.pnlAreaRange.Visibility = $(if ($isAreaRange) { 'Visible' } else { 'Collapsed' })"
    "  $c.pnlAreaMatch.Opacity = $(if ($on4) { 1.0 } else { 0.4 })"
    "  $c.pnlAreaRange.Opacity = $(if ($on4) { 1.0 } else { 0.4 })"
    "}"
    "Update-State"
    "$c.chkLt.Add_Click({ Update-State })"
    "$c.chkLw.Add_Click({ Update-State })"
    "$c.chkLen.Add_Click({ Update-State })"
    "$c.rbLenMatch.Add_Click({ Update-State })"
    "$c.rbLenRange.Add_Click({ Update-State })"
    "$c.chkAng.Add_Click({ Update-State })"
    "$c.rbAngMatch.Add_Click({ Update-State })"
    "$c.rbAngRange.Add_Click({ Update-State })"
    "$c.chkArea.Add_Click({ Update-State })"
    "$c.rbAreaMatch.Add_Click({ Update-State })"
    "$c.rbAreaRange.Add_Click({ Update-State })"
    "$c.areaAll.Add_Click({ foreach ($b in $arBoxes) { $b.IsChecked = $true } })"
    "$c.areaNone.Add_Click({ foreach ($b in $arBoxes) { $b.IsChecked = $false } })"
    "$c.angAll.Add_Click({ foreach ($b in $anBoxes) { $b.IsChecked = $true } })"
    "$c.angNone.Add_Click({ foreach ($b in $anBoxes) { $b.IsChecked = $false } })"
    "$c.rbSortStd.Add_Click({ Apply-Sort })"
    "$c.rbSortCnt.Add_Click({ Apply-Sort })"
    "$c.ltAll.Add_Click({ foreach ($b in $ltBoxes) { $b.IsChecked = $true } })"
    "$c.ltNone.Add_Click({ foreach ($b in $ltBoxes) { $b.IsChecked = $false } })"
    "$c.lwAll.Add_Click({ foreach ($b in $lwBoxes) { $b.IsChecked = $true } })"
    "$c.lwNone.Add_Click({ foreach ($b in $lwBoxes) { $b.IsChecked = $false } })"
    "$c.lenAll.Add_Click({ foreach ($b in $lenBoxes) { $b.IsChecked = $true } })"
    "$c.lenNone.Add_Click({ foreach ($b in $lenBoxes) { $b.IsChecked = $false } })"
    "function Set-Fold($key, $fold) {"
    "  $bd = $c['bd' + $key]"
    "  $ar = $c['ar' + $key]"
    "  if ($fold) {"
    "    $bd.Visibility = 'Collapsed'"
    "    $ar.RenderTransform = New-Object System.Windows.Media.RotateTransform(-90)"
    "  } else {"
    "    $bd.Visibility = 'Visible'"
    "    $ar.RenderTransform = New-Object System.Windows.Media.RotateTransform(0)"
    "  }"
    "}"
    "$c.tgLt.Add_Click({ Set-Fold 'Lt' ($c.bdLt.Visibility -eq 'Visible') })"
    "$c.tgLw.Add_Click({ Set-Fold 'Lw' ($c.bdLw.Visibility -eq 'Visible') })"
    "$c.tgLen.Add_Click({ Set-Fold 'Len' ($c.bdLen.Visibility -eq 'Visible') })"
    "$c.tgAng.Add_Click({ Set-Fold 'Ang' ($c.bdAng.Visibility -eq 'Visible') })"
    "$c.tgArea.Add_Click({ Set-Fold 'Area' ($c.bdArea.Visibility -eq 'Visible') })"
    "$c.btnX.Add_Click({ $win.Close() })"
    "$c.hdr.Add_MouseLeftButtonDown({ try { $win.DragMove() } catch { } })"
    "$win.Add_Loaded({ [void]$win.Activate() })"
    "$win.Add_MouseRightButtonUp({"
    "  param($s, $e)"
    "  $d = $e.OriginalSource"
    "  while ($d -ne $null) {"
    "    if ($d -is [System.Windows.Controls.TextBox]) { return }"
    "    if ($d -isnot [System.Windows.Media.Visual]) { break }"
    "    $d = [System.Windows.Media.VisualTreeHelper]::GetParent($d)"
    "  }"
    "  $ev = New-Object System.Windows.RoutedEventArgs"
    "  $ev.RoutedEvent = [System.Windows.Controls.Primitives.ButtonBase]::ClickEvent"
    "  $c.btnOk.RaiseEvent($ev)"
    "})"
    "$c.btnOk.Add_Click({"
    "  $c.txtMsg.Text = ''"
    "  $useLt = [bool]$c.chkLt.IsChecked"
    "  $useLw = [bool]$c.chkLw.IsChecked"
    "  $useLen = [bool]$c.chkLen.IsChecked"
    "  $useAng = [bool]$c.chkAng.IsChecked"
    "  $useArea = [bool]$c.chkArea.IsChecked"
    "  if (-not ($useLt -or $useLw -or $useLen -or $useAng -or $useArea)) {"
    "    $c.txtMsg.Text = '条件を1つ以上オンにしてください。'"
    "    return"
    "  }"
    "  $ltIdx = New-Object System.Collections.ArrayList"
    "  for ($i = 0; $i -lt $ltBoxes.Count; $i++) {"
    "    if ($ltBoxes[$i].IsChecked) { [void]$ltIdx.Add([int]$ltBoxes[$i].DataContext) }"
    "  }"
    "  $lwIdx = New-Object System.Collections.ArrayList"
    "  for ($i = 0; $i -lt $lwBoxes.Count; $i++) {"
    "    if ($lwBoxes[$i].IsChecked) { [void]$lwIdx.Add([int]$lwBoxes[$i].DataContext) }"
    "  }"
    "  $lenIdx = New-Object System.Collections.ArrayList"
    "  for ($i = 0; $i -lt $lenBoxes.Count; $i++) {"
    "    if ($lenBoxes[$i].IsChecked) { [void]$lenIdx.Add([int]$lenBoxes[$i].DataContext) }"
    "  }"
    "  if ($useLt -and $ltIdx.Count -eq 0) {"
    "    $c.txtMsg.Text = '線種を1つ以上チェックしてください。'"
    "    return"
    "  }"
    "  if ($useLw -and $lwIdx.Count -eq 0) {"
    "    $c.txtMsg.Text = '太さを1つ以上チェックしてください。'"
    "    return"
    "  }"
    "  $isRange = [bool]$c.rbLenRange.IsChecked"
    "  $lmodeVal = 'MATCH'"
    "  if ($isRange) { $lmodeVal = 'RANGE' }"
    "  if ($useLen -and (-not $isRange) -and $lenIdx.Count -eq 0) {"
    "    $c.txtMsg.Text = '長さを1つ以上チェックしてください。'"
    "    return"
    "  }"
    "  $nsf = [System.Globalization.NumberStyles]::Float"
    "  $tol = 0.0"
    "  $okTol = [double]::TryParse($c.txtTol.Text.Trim(), $nsf, $inv, [ref]$tol)"
    "  if ($useLen -and (-not $isRange) -and ((-not $okTol) -or $tol -lt 0)) {"
    "    $c.txtMsg.Text = '許容誤差は0以上の数値で入力してください。'"
    "    return"
    "  }"
    "  if (-not $okTol) { $tol = 0.001 }"
    "  $minT = $c.txtMin.Text.Trim()"
    "  $maxT = $c.txtMax.Text.Trim()"
    "  $vMin = 0.0"
    "  $vMax = 0.0"
    "  $okMin = ($minT -eq '') -or [double]::TryParse($minT, $nsf, $inv, [ref]$vMin)"
    "  $okMax = ($maxT -eq '') -or [double]::TryParse($maxT, $nsf, $inv, [ref]$vMax)"
    "  if ($useLen -and $isRange) {"
    "    if (-not ($okMin -and $okMax)) {"
    "      $c.txtMsg.Text = '以上・以下は数値で入力してください。'"
    "      return"
    "    }"
    "    if (($minT -eq '') -and ($maxT -eq '')) {"
    "      $c.txtMsg.Text = '以上・以下のどちらか一方は入力してください。'"
    "      return"
    "    }"
    "    if (($minT -ne '') -and ($maxT -ne '') -and ($vMin -gt $vMax)) {"
    "      $c.txtMsg.Text = '「以上」は「以下」以下の値にしてください。'"
    "      return"
    "    }"
    "  }"
    "  $minOut = ''"
    "  $maxOut = ''"
    "  if (($minT -ne '') -and $okMin) { $minOut = $vMin.ToString('0.########', $inv) }"
    "  if (($maxT -ne '') -and $okMax) { $maxOut = $vMax.ToString('0.########', $inv) }"
    "  $angIdx = New-Object System.Collections.ArrayList"
    "  for ($i = 0; $i -lt $anBoxes.Count; $i++) {"
    "    if ($anBoxes[$i].IsChecked) { [void]$angIdx.Add([int]$anBoxes[$i].DataContext) }"
    "  }"
    "  $isARange = [bool]$c.rbAngRange.IsChecked"
    "  $amodeVal = 'MATCH'"
    "  if ($isARange) { $amodeVal = 'RANGE' }"
    "  if ($useAng -and (-not $isARange) -and $angIdx.Count -eq 0) {"
    "    $c.txtMsg.Text = '角度を1つ以上チェックしてください。'"
    "    return"
    "  }"
    "  $atol = 0.0"
    "  $okATol = [double]::TryParse($c.txtATol.Text.Trim(), $nsf, $inv, [ref]$atol)"
    "  if ($useAng -and (-not $isARange) -and ((-not $okATol) -or $atol -lt 0)) {"
    "    $c.txtMsg.Text = '角度の許容誤差は0以上の数値で入力してください。'"
    "    return"
    "  }"
    "  if (-not $okATol) { $atol = 0.01 }"
    "  $aMinT = $c.txtAMin.Text.Trim()"
    "  $aMaxT = $c.txtAMax.Text.Trim()"
    "  $vAMin = 0.0"
    "  $vAMax = 0.0"
    "  $okAMin = ($aMinT -eq '') -or [double]::TryParse($aMinT, $nsf, $inv, [ref]$vAMin)"
    "  $okAMax = ($aMaxT -eq '') -or [double]::TryParse($aMaxT, $nsf, $inv, [ref]$vAMax)"
    "  if ($useAng -and $isARange) {"
    "    if (-not ($okAMin -and $okAMax)) {"
    "      $c.txtMsg.Text = '角度の以上・以下は数値で入力してください。'"
    "      return"
    "    }"
    "    if (($aMinT -eq '') -and ($aMaxT -eq '')) {"
    "      $c.txtMsg.Text = '角度の以上・以下のどちらか一方は入力してください。'"
    "      return"
    "    }"
    "    if (($aMinT -ne '') -and ($aMaxT -ne '') -and ($vAMin -gt $vAMax)) {"
    "      $c.txtMsg.Text = '角度の「以上」は「以下」以下の値にしてください。'"
    "      return"
    "    }"
    "  }"
    "  $aMinOut = ''"
    "  $aMaxOut = ''"
    "  if (($aMinT -ne '') -and $okAMin) { $aMinOut = $vAMin.ToString('0.########', $inv) }"
    "  if (($aMaxT -ne '') -and $okAMax) { $aMaxOut = $vAMax.ToString('0.########', $inv) }"
    "  $arIdx = New-Object System.Collections.ArrayList"
    "  for ($i = 0; $i -lt $arBoxes.Count; $i++) {"
    "    if ($arBoxes[$i].IsChecked) { [void]$arIdx.Add([int]$arBoxes[$i].DataContext) }"
    "  }"
    "  $isAreaRange = [bool]$c.rbAreaRange.IsChecked"
    "  $armodeVal = 'MATCH'"
    "  if ($isAreaRange) { $armodeVal = 'RANGE' }"
    "  if ($useArea -and (-not $isAreaRange) -and $arIdx.Count -eq 0) {"
    "    $c.txtMsg.Text = '面積を1つ以上チェックしてください。'"
    "    return"
    "  }"
    "  $artol = 0.0"
    "  $okArTol = [double]::TryParse($c.txtAreaTol.Text.Trim(), $nsf, $inv, [ref]$artol)"
    "  if ($useArea -and (-not $isAreaRange) -and ((-not $okArTol) -or $artol -lt 0)) {"
    "    $c.txtMsg.Text = '面積の許容誤差は0以上の数値で入力してください。'"
    "    return"
    "  }"
    "  if (-not $okArTol) { $artol = 0.001 }"
    "  $arMinT = $c.txtAreaMin.Text.Trim()"
    "  $arMaxT = $c.txtAreaMax.Text.Trim()"
    "  $vArMin = 0.0"
    "  $vArMax = 0.0"
    "  $okArMin = ($arMinT -eq '') -or [double]::TryParse($arMinT, $nsf, $inv, [ref]$vArMin)"
    "  $okArMax = ($arMaxT -eq '') -or [double]::TryParse($arMaxT, $nsf, $inv, [ref]$vArMax)"
    "  if ($useArea -and $isAreaRange) {"
    "    if (-not ($okArMin -and $okArMax)) {"
    "      $c.txtMsg.Text = '面積の以上・以下は数値で入力してください。'"
    "      return"
    "    }"
    "    if (($arMinT -eq '') -and ($arMaxT -eq '')) {"
    "      $c.txtMsg.Text = '面積の以上・以下のどちらか一方は入力してください。'"
    "      return"
    "    }"
    "    if (($arMinT -ne '') -and ($arMaxT -ne '') -and ($vArMin -gt $vArMax)) {"
    "      $c.txtMsg.Text = '面積の「以上」は「以下」以下の値にしてください。'"
    "      return"
    "    }"
    "  }"
    "  $arMinOut = ''"
    "  $arMaxOut = ''"
    "  if (($arMinT -ne '') -and $okArMin) { $arMinOut = $vArMin.ToString('0.########', $inv) }"
    "  if (($arMaxT -ne '') -and $okArMax) { $arMaxOut = $vArMax.ToString('0.########', $inv) }"
    "  $out = @("
    "    'OK=1',"
    "    ('USELT=' + [int]$useLt),"
    "    ('USELW=' + [int]$useLw),"
    "    ('USELEN=' + [int]$useLen),"
    "    ('TOL=' + $tol.ToString('0.########', $inv)),"
    "    ('LT=' + ($ltIdx -join ',')),"
    "    ('LW=' + ($lwIdx -join ',')),"
    "    ('LEN=' + ($lenIdx -join ',')),"
    "    ('LMODE=' + $lmodeVal),"
    "    ('LMIN=' + $minOut),"
    "    ('LMAX=' + $maxOut),"
    "    ('SORT=' + [int][bool]$c.rbSortCnt.IsChecked),"
    "    ('USEANG=' + [int]$useAng),"
    "    ('ANG=' + ($angIdx -join ',')),"
    "    ('AMODE=' + $amodeVal),"
    "    ('AMIN=' + $aMinOut),"
    "    ('AMAX=' + $aMaxOut),"
    "    ('ATOL=' + $atol.ToString('0.########', $inv)),"
    "    ('USEAR=' + [int]$useArea),"
    "    ('AR=' + ($arIdx -join ',')),"
    "    ('ARMODE=' + $armodeVal),"
    "    ('ARMIN=' + $arMinOut),"
    "    ('ARMAX=' + $arMaxOut),"
    "    ('ARTOL=' + $artol.ToString('0.########', $inv))"
    "  )"
    "  [System.IO.File]::WriteAllLines($ResPath, [string[]]$out, $enc)"
    "  $win.Close()"
    "})"
    "[void]$win.ShowDialog()"
    "} catch {"
    "  [System.IO.File]::WriteAllText($ResPath, ('ERR=' + $_.Exception.Message), $enc)"
    "}"
   )
)

(princ "\nコマンド「SQ」(線種・太さ・長さ・面積・角度選択)で実行できます。")
(princ)