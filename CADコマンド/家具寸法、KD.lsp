;;; ==========================================================================
;;; コマンド: KD (事前選択一括対応・完璧版)
;;; 目的: 基準の壁と複数ブロックを選択し、連続ピッチ寸法を自動生成。
;;;       SBW/SBX等のコマンドで事前選択したブロックをそのまま引き継ぎます。
;;; ==========================================================================
(defun c:KD ( / old_osmode old_cmdecho old_error dir wallEnt ssBlk 
                get-bounding-box is-block-width wallBox wallMin baseVal 
                defaultCrossVal crossVal extPt
                i blkList maxHigh minLow blkEnt bBox bMin bMax pts uniquePts 
                prev p1 p2 pt1 pt2 pt3 defaultDimPos dimPt dimLinePos dimEnt )
  
  ;; Visual LISP関数の有効化
  (vl-load-com)
  
  ;; =====================================================================
  ;; 【最重要】他の入力プロンプトで選択が解除される前に、事前選択を確保する
  ;; =====================================================================
  (setq ssBlk (ssget "_I" '((0 . "INSERT"))))
  (if ssBlk
    (sssetfirst nil) ;; 取得後はグリップ表示を解除（以降の画面クリックを見やすくするため）
  )
  
  ;; 元のエラーハンドラを退避し、一時的なエラーハンドラをグローバルに設定
  (setq old_error *error*)
  (setq *error* (lambda (msg)
                  (if old_osmode (setvar "OSMODE" old_osmode))
                  (if old_cmdecho (setvar "CMDECHO" old_cmdecho))
                  (setq *error* old_error)
                  (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
                    (princ (strcat "\nエラー: " msg))
                  )
                  (princ)
                ))

  ;; 環境変数の退避と設定
  (setq old_osmode (getvar "OSMODE"))
  (setq old_cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 1. 方向選択
  (initget "H V")
  (setq dir (getkword "\n寸法方向を選択 [水平(H)/垂直(V)] : "))
  (if (not dir) (setq dir "H"))

  ;; 2. 壁オブジェクトの選択
  (setq wallEnt (car (entsel "\n基準となる壁（線分/ポリライン等）を選択: ")))
  (if (not wallEnt)
    (progn
      (princ "\n壁が選択されませんでした。処理を中断します。")
      (exit)
    )
  )

  ;; 3. ブロックの選択（事前選択がない場合のみ、ユーザーに選択を促す）
  (if (not ssBlk)
    (progn
      (princ "\n対象となるブロックを選択: ")
      (setq ssBlk (ssget '((0 . "INSERT"))))
    )
  )
  
  ;; それでもブロックが無い場合は終了
  (if (not ssBlk)
    (progn
      (princ "\nブロックが選択されませんでした。処理を中断します。")
      (exit)
    )
  )
  
  ;; [ヘルパー関数] バウンディングボックス取得
  (defun get-bounding-box (ent / obj minPt maxPt)
    (setq obj (vlax-ename->vla-object ent))
    (vla-getboundingbox obj 'minPt 'maxPt)
    (list (trans (vlax-safearray->list minPt) 0 1)
          (trans (vlax-safearray->list maxPt) 0 1))
  )

  ;; [ヘルパー関数] 寸法がブロック自体の幅か判定する
  (defun is-block-width (ptA ptB / isBlk)
    (setq isBlk nil)
    (foreach b blkList
      (if (and (< (abs (- ptA (car b))) 1e-4)
               (< (abs (- ptB (cadr b))) 1e-4))
        (setq isBlk T)
      )
    )
    isBlk
  )

  ;; 4. 壁座標の解析
  (setq wallBox (get-bounding-box wallEnt))
  (setq wallMin (car wallBox))
  (if (= dir "H")
    (progn
      (setq baseVal (car wallMin))
      (setq defaultCrossVal (cadr wallMin))
    )
    (progn
      (setq baseVal (cadr wallMin))
      (setq defaultCrossVal (car wallMin))
    )
  )

  ;; 5. ブロックの座標解析とソート準備
  (setq i 0 blkList nil maxHigh -1e99 minLow 1e99)
  (while (< i (sslength ssBlk))
    (setq blkEnt (ssname ssBlk i))
    (setq bBox (get-bounding-box blkEnt))
    (setq bMin (car bBox) bMax (cadr bBox))
    
    (if (= dir "H")
      (progn
        (setq blkList (cons (list (car bMin) (car bMax)) blkList))
        (if (> (cadr bMax) maxHigh) (setq maxHigh (cadr bMax)))
      )
      (progn
        (setq blkList (cons (list (cadr bMin) (cadr bMax)) blkList))
        (if (< (car bMin) minLow) (setq minLow (car bMin)))
      )
    )
    (setq i (1+ i))
  )

  ;; 座標のソートと重複削除(0寸法の事前排除)
  (setq pts (list baseVal))
  (foreach b blkList
    (setq pts (cons (car b) pts))
    (setq pts (cons (cadr b) pts))
  )
  (setq pts (vl-sort pts '<))
  
  (setq uniquePts nil)
  (if pts
    (progn
      (setq uniquePts (list (car pts)))
      (setq prev (car pts))
      (foreach p (cdr pts)
        (if (> (abs (- p prev)) 1e-4)
          (progn
            (setq uniquePts (cons p uniquePts))
            (setq prev p)
          )
        )
      )
      (setq uniquePts (reverse uniquePts))
    )
  )

  ;; 6. 寸法補助線の起点 と 寸法線の配置位置 の指定
  (setq extPt (getpoint "\n寸法補助線の起点を画面クリックで指定 : "))
  (if extPt
    (if (= dir "H") (setq crossVal (cadr extPt)) (setq crossVal (car extPt)))
    (setq crossVal defaultCrossVal)
  )

  (if (= dir "H")
    (setq defaultDimPos maxHigh)
    (setq defaultDimPos minLow)
  )
  
  (setq dimPt (getpoint "\n寸法線の配置位置を画面クリックで指定 : "))
  (if dimPt
    (if (= dir "H") (setq dimLinePos (cadr dimPt)) (setq dimLinePos (car dimPt)))
    (setq dimLinePos defaultDimPos)
  )

  ;; 7. 連続寸法の生成と不要寸法の自動削除
  (setvar "OSMODE" 0)
  
  (setq i 0)
  (while (< i (1- (length uniquePts)))
    (setq p1 (nth i uniquePts))
    (setq p2 (nth (1+ i) uniquePts))
    
    (if (= dir "H")
      (progn
        (setq pt1 (list p1 crossVal 0.0))
        (setq pt2 (list p2 crossVal 0.0))
        (setq pt3 (list p1 dimLinePos 0.0))
      )
      (progn
        (setq pt1 (list crossVal p1 0.0))
        (setq pt2 (list crossVal p2 0.0))
        (setq pt3 (list dimLinePos p1 0.0))
      )
    )
    
    (command "_.DIMLINEAR" pt1 pt2 pt3)
    (setq dimEnt (entlast))
    
    (if (is-block-width p1 p2)
      (if (entget dimEnt) (entdel dimEnt))
    )
    
    (setq i (1+ i))
  )

  ;; 正常終了時の環境変数およびエラーハンドラの復元
  (setvar "OSMODE" old_osmode)
  (setvar "CMDECHO" old_cmdecho)
  (setq *error* old_error)
  
  (princ "\n処理が完了しました。")
  (princ)
)
(princ "\n[KDコマンド] 読込完了: 事前選択対応版")
(princ)