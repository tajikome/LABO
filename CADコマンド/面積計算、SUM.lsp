;;; ============================================================================
;;;  SUM.LSP
;;;  面積自動計算・テキスト配置ツール（内包オブジェクト自動減算対応）
;;;
;;;  コマンド : SUM
;;;  対象     : LWPOLYLINE / POLYLINE / HATCH / REGION / CIRCLE / ELLIPSE
;;;
;;;  概要:
;;;    1) 加算オブジェクト（外側の図形。複数選択可）を選択する
;;;    2) カレントスペース内の対象種別オブジェクトの中から、加算オブジェクトの
;;;       内側に完全に含まれるものを自動検出し、その面積を自動的に減算する
;;;    3) 配置位置をクリックすると、そこから Y方向に -350 (下へ350) ずらした
;;;       位置を基点として、合計面積を単一行文字（DText）で配置する
;;;       画層 i_MENSEKI_MOJI_1・文字スタイル i_MENSEKI_MOJI_1（MSゴシック）・
;;;       中央揃え(MC)・文字高さ250.0、「[数値]㎡」（小数第2位四捨五入）の書式
;;;       画層・文字スタイルとも、図面に存在しない場合は自動的に新規作成する
;;;
;;;  面積計算基準:
;;;    図面は 1000mm = 1m（1図面単位 = 1mm）を前提としており、
;;;    vla-get-Area で得られる面積（mm^2 相当）を 1,000,000 で除算して
;;;    m^2 に換算してから表記する。
;;;
;;;  文字スタイル・フォントについて:
;;;    出力専用の文字スタイル i_MENSEKI_MOJI_1 を用意し、フォントを
;;;    「MS Gothic」に設定する。既存の Standard 等、他のオブジェクトが
;;;    参照している可能性のあるスタイルは変更しない
;;;    （他の文字オブジェクトへの影響を避けるため）。
;;;
;;;  内包判定について:
;;;    加算オブジェクトの外形を２次元点列に近似し（曲線系はパラメータ分割で
;;;    サンプリング、HATCH/REGIONはバウンディングボックスの矩形で近似）、
;;;    候補オブジェクトのバウンディングボックスが完全に内包され、かつ候補の
;;;    中心点がその多角形内部にある場合に「内包」と判定して減算する。
;;;    そのため、極端な凹形状や複雑に重なり合う形状では判定精度が
;;;    低下する場合がある（面積そのものは vla-get-Area により正確に取得）。
;;;
;;;  読み込み方法:
;;;    APPLOAD コマンドで本ファイルをロードするか、AutoCAD の作図ウィンドウへ
;;;    ドラッグ＆ドロップしてください。その後コマンドラインに SUM と入力します。
;;; ============================================================================

(vl-load-com)

;; ----------------------------------------------------------------------------
;; SUM:pt-in-poly
;;   点 pt (x y) が、2D点列 poly（閉じた多角形とみなす）の内部にあるかを
;;   レイキャスティング法で判定する。
;; ----------------------------------------------------------------------------
(defun SUM:pt-in-poly (pt poly / n i j xi yi xj yj px py inside)
  (setq px (car pt) py (cadr pt))
  (setq n (length poly))
  (setq inside nil)
  (if (>= n 3)
    (progn
      (setq j (1- n))
      (setq i 0)
      (while (< i n)
        (setq xi (car  (nth i poly)))
        (setq yi (cadr (nth i poly)))
        (setq xj (car  (nth j poly)))
        (setq yj (cadr (nth j poly)))
        (if (and (not (eq (> yi py) (> yj py)))
                 (/= yj yi)
                 (< px (+ xi (/ (* (- xj xi) (- py yi)) (- yj yi)))))
          (setq inside (not inside))
        )
        (setq j i)
        (setq i (1+ i))
      )
    )
  )
  inside
)

;; ----------------------------------------------------------------------------
;; SUM:get-bbox
;;   VLAオブジェクトのワールド座標バウンディングボックスを
;;   ( (minx miny minz) (maxx maxy maxz) ) の形式で返す。取得不可の場合は nil。
;; ----------------------------------------------------------------------------
(defun SUM:get-bbox (obj / bb1 bb2 err)
  (setq bb1 nil bb2 nil)
  (setq err (vl-catch-all-apply 'vla-getboundingbox (list obj 'bb1 'bb2)))
  (if (or (vl-catch-all-error-p err) (not bb1) (not bb2))
    nil
    (list (vlax-safearray->list bb1) (vlax-safearray->list bb2))
  )
)

;; ----------------------------------------------------------------------------
;; SUM:bbox-inside-p
;;   bbA が bbB に完全に内包されているか（XY平面のみで判定）
;; ----------------------------------------------------------------------------
(defun SUM:bbox-inside-p (bbA bbB / minA maxA minB maxB)
  (setq minA (car bbA) maxA (cadr bbA))
  (setq minB (car bbB) maxB (cadr bbB))
  (and (>= (car  minA) (car  minB))
       (>= (cadr minA) (cadr minB))
       (<= (car  maxA) (car  maxB))
       (<= (cadr maxA) (cadr maxB)))
)

;; ----------------------------------------------------------------------------
;; SUM:get-poly-points
;;   オブジェクトの外形を近似する2D点列（XY）を返す。
;;   ・LWPOLYLINE / POLYLINE / CIRCLE / ELLIPSE : パラメータ分割サンプリング
;;   ・HATCH / REGION 等その他                   : バウンディングボックスの矩形で近似
;; ----------------------------------------------------------------------------
(defun SUM:get-poly-points (obj / etype pts sp ep step t1 cnt pt3 bbox mn mx)
  (setq etype (vla-get-ObjectName obj))
  (setq pts '())
  (cond
    ((member etype '("AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline"
                      "AcDbCircle" "AcDbEllipse"))
     (setq sp nil ep nil)
     (vl-catch-all-apply
       '(lambda ()
          (setq sp (vlax-curve-getStartParam obj))
          (setq ep (vlax-curve-getEndParam obj))
        )
     )
     (if (and sp ep (> ep sp))
       (progn
         (setq cnt 72)
         (setq step (/ (- ep sp) (float cnt)))
         (setq t1 sp)
         (repeat (1+ cnt)
           (setq pt3 nil)
           (vl-catch-all-apply
             '(lambda () (setq pt3 (vlax-curve-getPointAtParam obj (min t1 ep))))
           )
           (if pt3 (setq pts (cons (list (car pt3) (cadr pt3)) pts)))
           (setq t1 (+ t1 step))
         )
       )
     )
    )
    (t
     (setq bbox (SUM:get-bbox obj))
     (if bbox
       (progn
         (setq mn (car bbox) mx (cadr bbox))
         (setq pts (list (list (car mn) (cadr mn))
                          (list (car mx) (cadr mn))
                          (list (car mx) (cadr mx))
                          (list (car mn) (cadr mx))))
       )
     )
    )
  )
  (reverse pts)
)

;; ----------------------------------------------------------------------------
;; SUM:ensure-layer
;;   layerName の画層が図面に存在しなければ新規作成する。
;; ----------------------------------------------------------------------------
(defun SUM:ensure-layer (doc layerName / layers)
  (if (not (tblsearch "LAYER" layerName))
    (progn
      (setq layers (vla-get-Layers doc))
      (vl-catch-all-apply 'vla-add (list layers layerName))
    )
  )
  layerName
)

;; ----------------------------------------------------------------------------
;; SUM:ensure-style
;;   styleName の文字スタイルを確保し、フォントを typeface に設定する。
;;   既に存在する場合はそのスタイルのフォントを typeface に更新する。
;;   （Standard 等、他のオブジェクトが参照している可能性のあるスタイルは
;;     このスタイル名を渡さない限り一切変更しない）
;; ----------------------------------------------------------------------------
(defun SUM:ensure-style (doc styleName typeface / styles sty result)
  (setq styles (vla-get-TextStyles doc))
  (setq sty nil)
  (setq result (vl-catch-all-apply 'vla-item (list styles styleName)))
  (if (not (vl-catch-all-error-p result))
    (setq sty result)
    (progn
      (setq result (vl-catch-all-apply 'vla-add (list styles styleName)))
      (if (not (vl-catch-all-error-p result))
        (setq sty result)
      )
    )
  )
  (if sty
    (vl-catch-all-apply
      'vla-SetFont
      (list sty typeface :vlax-false :vlax-false 128 0)
    )
  )
  styleName
)

;; ----------------------------------------------------------------------------
;; メインコマンド : SUM
;; ----------------------------------------------------------------------------
(defun c:SUM ( / *error* doc space filterStr layerName styleName styleFont
                 ss1 ss2 n i outerList candList grossTotal totalInner
                 netTotal netTotalM2 outerEnt outerObj outerArea bbOuter
                 polyOuter innerEnt innerObj bbInner centerInner isIn subArea
                 insPt placePt txtStr txtObj )

  (defun *error* (msg)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*")))
      (princ (strcat "\n[SUM] エラー: " msg))
    )
    (princ)
  )

  (vl-load-com)
  (setq doc   (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq space (vla-get-Block (vla-get-ActiveLayout doc)))

  (setq filterStr  "LWPOLYLINE,POLYLINE,HATCH,REGION,CIRCLE,ELLIPSE")
  (setq layerName  "i_MENSEKI_MOJI_1")
  (setq styleName  "i_MENSEKI_MOJI_1")
  (setq styleFont  "MS Gothic")

  ;; 1) 加算オブジェクトの選択 -------------------------------------------
  (princ "\n加算するオブジェクトを選択:")
  (setq ss1 (ssget (list (cons 0 filterStr))))

  (if (not ss1)
    (progn (princ "\nオブジェクトが選択されませんでした。") (princ))

    (progn
      (setq outerList '())
      (setq n (sslength ss1) i 0)
      (repeat n
        (setq outerList (cons (ssname ss1 i) outerList))
        (setq i (1+ i))
      )

      ;; カレントスペース内の対象種別オブジェクトを全て取得（減算候補プール）
      (setq candList '())
      (setq ss2 (ssget "X" (list (cons 0 filterStr))))
      (if ss2
        (progn
          (setq n (sslength ss2) i 0)
          (repeat n
            (setq candList (cons (ssname ss2 i) candList))
            (setq i (1+ i))
          )
        )
      )
      ;; 加算オブジェクト自身は減算候補から除外
      (foreach e outerList (setq candList (vl-remove e candList)))

      (setq grossTotal 0.0)
      (setq totalInner 0.0)

      ;; 2) 加算オブジェクトごとに面積を集計し、内包図形を検出して減算 -----
      (foreach outerEnt outerList
        (setq outerObj (vlax-ename->vla-object outerEnt))
        (setq outerArea 0.0)
        (vl-catch-all-apply '(lambda () (setq outerArea (vla-get-Area outerObj))))
        (setq grossTotal (+ grossTotal outerArea))

        (setq bbOuter   (SUM:get-bbox outerObj))
        (setq polyOuter (SUM:get-poly-points outerObj))

        (if (and bbOuter polyOuter (>= (length polyOuter) 3))
          (foreach innerEnt candList
            (setq innerObj (vlax-ename->vla-object innerEnt))
            (setq bbInner (SUM:get-bbox innerObj))
            (if (and bbInner (SUM:bbox-inside-p bbInner bbOuter))
              (progn
                (setq centerInner
                       (list (/ (+ (car (car bbInner)) (car (cadr bbInner))) 2.0)
                             (/ (+ (cadr (car bbInner)) (cadr (cadr bbInner))) 2.0)))
                (setq isIn (SUM:pt-in-poly centerInner polyOuter))
                (if isIn
                  (progn
                    (setq subArea 0.0)
                    (vl-catch-all-apply '(lambda () (setq subArea (vla-get-Area innerObj))))
                    (setq totalInner (+ totalInner subArea))
                    ;; 二重減算防止のため候補プールから除去
                    (setq candList (vl-remove innerEnt candList))
                  )
                )
              )
            )
          )
        )
      )

      ;; mm^2 で集計 → クランプ → m^2 へ換算(÷1,000,000)
      (setq netTotal (- grossTotal totalInner))
      (if (< netTotal 0.0) (setq netTotal 0.0))
      (setq netTotalM2 (/ netTotal 1000000.0))

      ;; 3) 配置位置の指定と単一行文字の配置 ------------------------------
      (setq insPt (getpoint "\n文字の配置位置をクリック: "))

      (if insPt
        (progn
          ;; クリック位置から Y方向へ -350 (下へ350) オフセットした点を配置基点とする
          (setq placePt
                 (list (car insPt)
                       (- (cadr insPt) 350.0)
                       (if (caddr insPt) (caddr insPt) 0.0)))

          (setq txtStr (strcat (rtos netTotalM2 2 2) "㎡"))

          ;; 出力画層・文字スタイル(MSゴシック)の確認・自動作成
          (SUM:ensure-layer doc layerName)
          (SUM:ensure-style doc styleName styleFont)

          ;; 単一行文字（DText）の作成：中央揃え(MC)・高さ250.0
          (setq txtObj (vla-AddText space txtStr (vlax-3d-point placePt) 250.0))
          (vla-put-Layer txtObj layerName)
          (vla-put-StyleName txtObj styleName)
          (vla-put-Alignment txtObj 10)   ; 10 = acAlignmentMiddleCenter (MC)
          (vla-put-TextAlignmentPoint txtObj (vlax-3d-point placePt))

          (princ (strcat "\n配置しました → " txtStr
                          "  (加算合計: " (rtos (/ grossTotal 1000000.0) 2 2)
                          "㎡ / 減算合計: " (rtos (/ totalInner 1000000.0) 2 2) "㎡)"))
        )
        (princ "\n配置位置が指定されなかったため中止しました。")
      )
    )
  )
  (princ)
)

(princ "\n[SUM] コマンドを読み込みました。実行するには「SUM」と入力してください。")
(princ)