;;; 文字配置_MJ.lsp
;;; クリック位置から指定方向へ120離して文字を配置し、方向に応じて回転角度を変える
;;; 配置後に文字編集(DDEDIT)が開き、その場で文字内容を入力する
(vl-load-com)

(defun c:MJ ( / *error* pt dpt dx dy ang rot dist pos wpos stl lay ent res ed)
  ;; ---- 設定 ----
  (setq lay  "L-MOJI")   ; 画層
  (setq stl  "7")        ; 文字スタイル
  (setq dist 120.0)      ; クリック位置からの距離

  (if (null *MJ-REV*) (setq *MJ-REV* nil)) ; nil=標準 / T=反転
  (if (null *MJ-TXT*) (setq *MJ-TXT* "文字")) ; 編集前の仮文字(前回入力値)

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\nエラー: " msg)))
    (princ))

  ;; 画層がなければ作成
  (if (null (tblsearch "LAYER" lay))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 lay) '(70 . 0) '(62 . 7) '(6 . "Continuous"))))
  ;; 文字スタイルがなければ現在のスタイルで代用
  (if (null (tblsearch "STYLE" stl))
    (progn
      (princ (strcat "\n文字スタイル「" stl "」がないため現在のスタイルを使用します。"))
      (setq stl (getvar "TEXTSTYLE"))))

  ;; 配置ループ
  (while
    (progn
      (initget "R")
      (setq pt (getpoint
                 (strcat "\nクリック位置を指定 [回転反転(R)] <現在:"
                         (if *MJ-REV* "反転(X+=90/X-=270)" "標準(X+=270/X-=90)")
                         ">: ")))
      (cond
        ((= pt "R")                         ; 反転切替
         (setq *MJ-REV* (not *MJ-REV*))
         (princ (if *MJ-REV*
                  "\n→ 反転: X+方向=90 / X-方向=270"
                  "\n→ 標準: X+方向=270 / X-方向=90"))
         T)
        ((null pt) nil)                     ; Enter/ESCで終了
        (T
         (setq dpt (getpoint pt "\n配置方向をカーソルで指定: "))
         (if dpt
           (progn
             (setq dx (- (car dpt) (car pt))
                   dy (- (cadr dpt) (cadr pt)))
             (if (and (equal dx 0.0 1e-9) (equal dy 0.0 1e-9))
               (princ "\n方向が定まりません。")
               (progn
                 (if (>= (abs dx) (abs dy))
                   ;; X方向
                   (progn
                     (setq pos (list (+ (car pt) (if (> dx 0) dist (- dist)))
                                     (cadr pt) (caddr pt)))
                     (setq rot (if (> dx 0)
                                 (if *MJ-REV* 90.0 270.0)
                                 (if *MJ-REV* 270.0 90.0))))
                   ;; Y方向(回転は0。変更したい場合はここを書き換え)
                   (progn
                     (setq pos (list (car pt)
                                     (+ (cadr pt) (if (> dy 0) dist (- dist)))
                                     (caddr pt)))
                     (setq rot 0.0)))

                 ;; UCS対応
                 (setq ang  (+ (* rot (/ pi 180.0))
                               (angle '(0 0 0) (trans '(1 0 0) 1 0 T))))
                 (setq wpos (trans pos 1 0))

                 ;; 仮文字を配置
                 (entmake
                   (list '(0 . "TEXT")
                         (cons 8 lay)
                         (cons 10 wpos)
                         (cons 40 150.0)
                         (cons 1 *MJ-TXT*)
                         (cons 50 ang)
                         (cons 41 0.7)
                         (cons 51 0.0)
                         (cons 7 stl)
                         '(72 . 4)           ; 中央(M)
                         (cons 11 wpos)
                         '(73 . 0)))
                 (setq ent (entlast))

                 ;; 配置した文字を編集(その場で文字入力)
                 (princ "\n文字内容を入力してください(ESCで取り消し)")
                 (setq res (vl-catch-all-apply
                             '(lambda () (command "_.DDEDIT" ent ""))))
                 (if (vl-catch-all-error-p res)
                   ;; 取り消し: 仮文字を削除してループ終了
                   (progn
                     (if (entget ent) (entdel ent))
                     (princ "\n取り消しました。")
                     (setq dpt nil))
                   ;; 確定: 入力内容を次回の仮文字として記憶
                   (if (setq ed (entget ent))
                     (setq *MJ-TXT* (cdr (assoc 1 ed))))))
             )
             ;; dptがnilなら終了
             (if dpt T nil))
           nil))))
    )
  (princ))