(defun c:SR ( / ss i ent obj dxf objType p1 p2 len ang newAng coords newCoords pt1 pt2)
  (vl-load-com)
  (princ "\n--- [SR] 複数図形（線分・ポリライン）角度一括統一ツール ---")
  
  ;; LINE および LWPOLYLINE を選択対象にする
  (if (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
    (progn
      (setq ang (getreal "\n統一する絶対角度（度数法、例: 0, 90, 45）を入力: "))
      (if ang
        (progn
          (setq newAng (* ang (/ pi 180.0))) ; 度数法からラジアンへ変換
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq obj (vlax-ename->vla-object ent))
            (setq objType (vla-get-ObjectName obj))
            
            ;; 1. 通常の線分 (AcDbLine) の場合
            (if (= objType "AcDbLine")
              (progn
                (setq p1 (vlax-get obj 'StartPoint))
                (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
                (setq p2 (polar p1 newAng len))
                (vla-put-EndPoint obj (vlax-3d-point p2))
              )
            )
            
            ;; 2. 2点間（セグメント）のポリライン (AcDbPolyline) の場合への拡張処理
            (if (= objType "AcDbPolyline")
              (progn
                ;; ポリラインの頂点数が2（＝実質1セグメント）の場合を想定して始点・終点を処理
                (setq coords (vlax-get obj 'Coordinates))
                (if (= (length coords) 4) ; 2D座標で2点（X1, Y1, X2, Y2）の場合
                  (progn
                    (setq pt1 (list (nth 0 coords) (nth 1 coords) 0.0))
                    (setq pt2 (list (nth 2 coords) (nth 3 coords) 0.0))
                    (setq len (distance pt1 pt2))
                    (setq p2 (polar pt1 newAng len))
                    (setq newCoords (list (car pt1) (cadr pt1) (car p2) (cadr p2)))
                    (vlax-put obj 'Coordinates newCoords)
                  )
                )
              )
            )
            
            (setq i (1+ i))
          )
          (princ (strcat "\n" (itoa (sslength ss)) " 件の要素の角度を " (rtos ang 2 2) " 度に変更しました。"))
        )
      )
    )
    (princ "\n対象となる線分またはポリラインが選択されませんでした。")
  )
  (princ)
)