;;; ADM.lsp : 通過線にあたる図形ごとに直列寸法を自動作図
;;; コマンド: ADM
;;; 設定: 補助線オフセット(DIMEXO)=3 / 補助線延長(DIMEXE)=0 / 矢印サイズ(DIMASZ)=1
(vl-load-com)

(defun c:ADM ( / *error* doc ms oldcm oldos oldexo oldexe oldasz oldort ok
                 dir ss i obj mn mx x0 y0 x1 y1 m pt dp tmp res lst vals
                 uniq a b mid)

  (defun *error* (msg)
    (if (and tmp (not (vlax-erased-p tmp))) (vla-delete tmp))
    (if oldcm  (setvar "CMDECHO" oldcm))
    (if oldos  (setvar "OSMODE"  oldos))
    (if oldexo (setvar "DIMEXO"  oldexo))
    (if oldexe (setvar "DIMEXE"  oldexe))
    (if oldasz (setvar "DIMASZ"  oldasz))
    (if oldort (setvar "ORTHOMODE" oldort))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nエラー: " msg)))
    (princ))

  (setq doc    (vla-get-activedocument (vlax-get-acad-object))
        ms     (vla-get-modelspace doc)
        oldcm  (getvar "CMDECHO")
        oldos  (getvar "OSMODE")
        oldexo (getvar "DIMEXO")
        oldexe (getvar "DIMEXE")
        oldasz (getvar "DIMASZ")
        oldort (getvar "ORTHOMODE"))

  ;; 方向選択(前回値を記憶)
  (or *adm-dir* (setq *adm-dir* "H"))
  (initget "H V")
  (setq dir (getkword (strcat "\n方向を選択 [水平(H)/垂直(V)] <" *adm-dir* ">: ")))
  (if dir (setq *adm-dir* dir) (setq dir *adm-dir*))

  (princ "\n対象図形を選択: ")
  (if (setq ss (ssget '((0 . "LINE,ARC,CIRCLE,ELLIPSE,LWPOLYLINE,POLYLINE,SPLINE"))))
    (progn
      ;; 選択範囲のバウンディングボックス
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (vla-getboundingbox obj 'mn 'mx)
        (setq mn (vlax-safearray->list mn)
              mx (vlax-safearray->list mx))
        (setq x0 (if x0 (min x0 (car mn))  (car mn))
              y0 (if y0 (min y0 (cadr mn)) (cadr mn))
              x1 (if x1 (max x1 (car mx))  (car mx))
              y1 (if y1 (max y1 (cadr mx)) (cadr mx)))
        (setq i (1+ i)))
      (setq m (* 0.1 (max (- x1 x0) (- y1 y0) 1.0)))

      (if (setq pt (getpoint "\n通過線の位置を指示: "))
        (if (progn
              ;; 通過線の位置を基点に、カーソルで方向を示し数値入力(またはクリック)で位置指定
              (setvar "ORTHOMODE" 1)
              (setq ok nil)
              (while (not ok)
                (setq dp (getpoint pt "\n寸法線の位置を指示 (カーソルで方向を示して距離を入力も可): "))
                (cond
                  ((null dp) (setq ok T))
                  ((> (abs (if (= dir "H") (- (cadr dp) (cadr pt)) (- (car dp) (car pt)))) 1e-9)
                   (setq ok T))
                  (T (princ "\n距離が0です。寸法線を置く方向(上下/左右)へカーソルを動かしてください。"))))
              (setvar "ORTHOMODE" oldort)
              dp)
          (progn
            (setvar "CMDECHO" 0)

            ;; 一時的な走査線
            (setq tmp
              (if (= dir "H")
                (vla-addline ms
                  (vlax-3d-point (list (- x0 m) (cadr pt) 0.0))
                  (vlax-3d-point (list (+ x1 m) (cadr pt) 0.0)))
                (vla-addline ms
                  (vlax-3d-point (list (car pt) (- y0 m) 0.0))
                  (vlax-3d-point (list (car pt) (+ y1 m) 0.0)))))

            ;; 交点収集(水平→X座標 / 垂直→Y座標)
            (setq i 0 vals nil)
            (while (< i (sslength ss))
              (setq obj (vlax-ename->vla-object (ssname ss i)))
              (setq res (vl-catch-all-apply 'vla-intersectwith (list tmp obj 0)))
              (if (and (not (vl-catch-all-error-p res))
                       (= (type res) 'VARIANT)
                       (>= (vlax-variant-type res) 8192))
                (progn
                  (setq lst (vlax-safearray->list (vlax-variant-value res)))
                  (while (>= (length lst) 3)
                    (setq vals (cons (if (= dir "H") (car lst) (cadr lst)) vals)
                          lst  (cdddr lst)))))
              (setq i (1+ i)))
            (vla-delete tmp)
            (setq tmp nil)

            ;; 並べ替え+重複除去
            (setq vals (vl-sort vals '<) uniq nil)
            (foreach v vals
              (if (or (null uniq) (> (abs (- v (car uniq))) 1e-6))
                (setq uniq (cons v uniq))))
            (setq uniq (reverse uniq))

            ;; 寸法設定
            (setvar "OSMODE" 0)
            (setvar "DIMEXO" 3.0)   ; 寸法補助線のオフセット
            (setvar "DIMEXE" 0.0)   ; 寸法補助線の延長長さ
            (setvar "DIMASZ" 1.0)   ; 矢印サイズ

            ;; 隣り合う交点間に寸法
            (if (>= (length uniq) 2)
              (progn
                (setq i 0)
                (while (< i (1- (length uniq)))
                  (setq a (nth i uniq)
                        b (nth (1+ i) uniq)
                        mid (/ (+ a b) 2.0))
                  (if (= dir "H")
                    (command "_.dimlinear"
                             (list a (cadr pt) 0.0) (list b (cadr pt) 0.0)
                             "_H" (list mid (cadr dp) 0.0))
                    (command "_.dimlinear"
                             (list (car pt) a 0.0) (list (car pt) b 0.0)
                             "_V" (list (car dp) mid 0.0)))
                  (setq i (1+ i)))
                (princ (strcat "\n" (itoa (1- (length uniq))) " 個の寸法を作図しました。")))
              (princ "\n交点が2点未満のため寸法を作図できません。"))

            ;; 設定を元に戻す
            (setvar "OSMODE" oldos)
            (setvar "DIMEXO" oldexo)
            (setvar "DIMEXE" oldexe)
            (setvar "DIMASZ" oldasz)
            (setvar "CMDECHO" oldcm))))))
  (princ))

(princ "\nADM: 寸法自動作図 を読み込みました。コマンド名は ADM です。")
(princ)