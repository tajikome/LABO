;;; =========================================================================
;;; コマンド名: ALL-IN-DIM
;;; 概要: 部屋の閉じたポリラインを選択し、全周の内寸法線を一括作図します。
;;; =========================================================================

(defun c:ALL-IN-DIM ( / ent obj closed pts i pt1 pt2 mid-pt ang dim-ang dim-pt offset-dist side-choice)
  (vl-load-com)
  (princ "\n--- 部屋全周内寸一括作図コマンド ---")
  
  ;; 1. LWPOLYLINE の選択
  (setq ent (car (entsel "\n部屋の内郭ポリライン(LWPOLYLINE)を選択: ")))
  
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      
      ;; 寸法線のオフセット距離の設定（ミリ単位。必要に応じて調整してください）
      (setq offset-dist 150.0)
      
      ;; 頂点リストの取得
      (setq pts (get-lwpoly-verts obj))
      
      ;; 閉じているポリラインの場合、始点を末尾に追加してループを完結させる
      (setq closed (vlax-get-property obj 'Closed))
      (if (or (= closed :vlax-true) (= closed -1))
        (setq pts (append pts (list (car pts))))
      )
      
      ;; システム変数の保持と変更（スナップの影響を防ぐため OSMODE を一時オフ）
      (setvar "CMDECHO" 0)
      (command "_.undo" "_group")
      
      ;; 寸法線を描くループ
      (setq i 0)
      (while (< i (1- (length pts)))
        (setq pt1 (nth i pts)
              pt2 (nth (1+ i) pts))
        
        ;; 2点間距離が極小でない場合のみ作図
        (if (> (distance pt1 pt2) 1.0)
          (progn
            ;; 中点と角度の計算
            (setq mid-pt (list (/ (+ (car pt1) (car pt2)) 2.0)
                               (/ (+ (cadr pt1) (cadr pt2)) 2.0)
                               0.0))
            (setq ang (angle pt1 pt2))
            
            ;; 辺に対して垂直な方向（90度オフセット）に寸法線を配置
            (setq dim-ang (+ ang (/ pi 2.0)))
            (setq dim-pt (polar mid-pt dim-ang offset-dist))
            
            ;; 整列寸法 (DIMALIGNED) の作図
            (command "_.dimaligned" "_non" pt1 "_non" pt2 "_non" dim-pt)
          )
        )
        (setq i (1+ i))
      )
      
      (command "_.undo" "_end")
      (setvar "CMDECHO" 1)
      (princ (strcat "\n計 " (itoa (1- (length pts))) " 箇所の寸法線を作成しました。"))
    )
    (princ "\nエラー: 選択されたオブジェクトは LWPOLYLINE ではありません。")
  )
  (princ)
)

;;; -------------------------------------------------------------------------
;;; 補助関数: LWPOLYLINE の全頂点座標(2D)を取得
;;; -------------------------------------------------------------------------
(defun get-lwpoly-verts (vla-obj / raw-coords pts i)
  (setq raw-coords (vlax-safearray->list (vlax-variant-value (vla-get-coordinates vla-obj))))
  (setq i 0)
  (while (< i (length raw-coords))
    (setq pts (append pts (list (list (nth i raw-coords) (nth (1+ i) raw-coords) 0.0))))
    (setq i (+ i 2))
  )
  pts
)