(defun c:SDS (/ *error* check-layer oldCmdecho oldLayer oldDimlayer oldDimexo res dColor overrideColor pt1 pt2 ptLoc curPt loop set-prop history lineEnt dimEnt lastOp get-dim-color calc-ptLoc color-name offDist)
  
  ;; ==========================================
  ;;  設定エリア
  ;;  プラス値 (例: 500)  : 進行方向の「右手側」
  ;;  マイナス値(例: -500) : 進行方向の「左手側」
  ;; ==========================================
  (setq offDist 500) ; オフセット距離

  ;; --- エラー発生時でも必ず元の状態に戻すための設定 ---
  (defun *error* (msg)
    (if oldLayer (setvar "CLAYER" oldLayer))
    (if oldDimlayer (setvar "DIMLAYER" oldDimlayer))
    (if oldDimexo (setvar "DIMEXO" oldDimexo))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (if msg (princ (strcat "\nキャンセルされました: " msg)))
    (princ)
  )

  (setq oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (setq oldLayer (getvar "CLAYER"))
  
  ;; 寸法補助線のオフセット（DIMEXO）を「3」に設定
  (setq oldDimexo (getvar "DIMEXO"))
  (setvar "DIMEXO" 3)
  
  (if (getvar "DIMLAYER")
    (progn
      (setq oldDimlayer (getvar "DIMLAYER"))
      (setvar "DIMLAYER" ".")
    )
  )

  ;; --- 画層管理関数 (寸法用) ---
  (defun check-layer ()
    (if (not (tblsearch "LAYER" "実測寸法"))
      (command "._-LAYER" "_M" "実測寸法" "_C" "6" "" "")
      (command "._-LAYER" "_S" "実測寸法" "")
    )
    (setvar "CECOLOR" "BYLAYER")
  )

  ;; --- 色名を返す関数 ---
  (defun color-name (c)
    (cond 
      ((= c "1") "赤") ((= c "2") "黄") ((= c "3") "緑") ((= c "4") "水色") 
      ((= c "5") "青") ((= c "6") "紫") ((= c "7") "白黒") (T "ByLayer")
    )
  )

  ;; --- DXFデータを直接書き換えて画層と色を確実に固定する関数 ---
  (defun set-prop (ent lay col / ed colNum)
    (if (and ent (entget ent))
      (progn
        (setq ed (entget ent))
        (setq ed (subst (cons 8 lay) (assoc 8 ed) ed))
        (setq colNum (if (= col "BYLAYER") 256 (atoi col)))
        (if (assoc 62 ed)
          (setq ed (subst (cons 62 colNum) (assoc 62 ed) ed))
          (setq ed (append ed (list (cons 62 colNum))))
        )
        (entmod ed)
        (entupd ent)
      )
    )
  )

  ;; --- X方向/Y方向を判定して自動で色を返す関数 (X:黄 / Y:ByLayer) ---
  (defun get-dim-color (p1 p2 / dx dy)
    (setq dx (abs (- (car p2) (car p1))))
    (setq dy (abs (- (cadr p2) (cadr p1))))
    (if (> dx dy)
      "2"         ; X方向 (水平) -> 黄色
      "BYLAYER"   ; Y方向 (垂直) -> ByLayer
    )
  )

  ;; --- 寸法配置点を自動計算する関数 (進行方向の「右手側」に配置) ---
  (defun calc-ptLoc (p1 p2 / dx dy mid)
    (setq dx (- (car p2) (car p1)))
    (setq dy (- (cadr p2) (cadr p1)))
    (setq mid (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0)))
    (if (>= (abs dx) (abs dy))
      ;; X方向（水平）
      (if (> dx 0)
        (list (car mid) (- (cadr p1) offDist)) ; 左->右: 下側
        (list (car mid) (+ (cadr p1) offDist)) ; 右->左: 上側
      )
      ;; Y方向（垂直）
      (if (> dy 0)
        (list (+ (car p1) offDist) (cadr mid)) ; 下->上: 右側
        (list (- (car p1) offDist) (cadr mid)) ; 上->下: 左側
      )
    )
  )

  ;; 1. 最初の段階での色選択
  (initget "A R Y G C B M W BYL")
  (setq res (getkword "\n初期寸法線の色を選択 [自動(A)/赤(R)/黄(Y)/緑(G)/水色(C)/青(B)/紫(M)/白黒(W)/ByLayer(ByL)] : "))
  
  (setq overrideColor 
    (cond 
      ((= res "R") "1")
      ((= res "Y") "2")
      ((= res "G") "3")
      ((= res "C") "4")
      ((= res "B") "5")
      ((= res "M") "6")
      ((= res "W") "7")
      ((= res "BYL") "BYLAYER")
      (T nil)
    )
  )

  ;; 2. 線分と寸法の作図スタート
  (if (and (setq pt1 (getpoint "\n1点目: "))
           (setq pt2 (getpoint pt1 "\n2点目: ")))
    (progn
      (setq ptLoc (calc-ptLoc pt1 pt2))
      (setq dColor (if overrideColor overrideColor (get-dim-color pt1 pt2)))

      ;; --- 最初の線分作図 (0画層で描画) ---
      (setvar "CLAYER" "0")
      (command "_.LINE" "_non" pt1 "_non" pt2 "")
      (setq lineEnt (entlast))

      ;; --- 最初の寸法作図 (実測寸法画層で描画) ---
      (check-layer)
      (command "_.DIMLINEAR" "_non" pt1 "_non" pt2 "_non" ptLoc)
      (setq dimEnt (entlast))
      (set-prop dimEnt "実測寸法" dColor)
      
      (setq history (list (list pt1 pt2 lineEnt dimEnt)))
      (setq curPt pt2 loop T)

      (while loop
        (initget "R Y G C B M W BYL A U")
        (setq res (getpoint curPt 
          (strcat "\n[現在色:" (if overrideColor (strcat "手動(" (color-name overrideColor) ")") "自動判定") 
                  "] 次点指定 or 色変更(R/Y/G/C/B/M/W/ByL) 自動に戻す(A) 戻る(U): ")))
        
        (cond
          ((member res '("R" "Y" "G" "C" "B" "M" "W" "BYL"))
            (setq overrideColor (cond 
              ((= res "R") "1") ((= res "Y") "2") ((= res "G") "3") ((= res "C") "4") 
              ((= res "B") "5") ((= res "M") "6") ((= res "W") "7") (T "BYLAYER")))
            (princ (strcat "\n-> 寸法の色を [" (color-name overrideColor) "] に変更しました。"))
          )
          ((= res "A")
            (setq overrideColor nil)
            (princ "\n-> 寸法の色を自動判定モードに戻しました。")
          )
          ((= res "U")
            (if history
              (progn
                (setq lastOp (car history))
                (if (entget (caddr lastOp)) (entdel (caddr lastOp)))
                (if (entget (cadddr lastOp)) (entdel (cadddr lastOp)))
                (setq history (cdr history))
                (setq curPt (car lastOp))
              )
              (princ "\nこれ以上戻れません。")
            )
          )
          ((= (type res) 'LIST)
            (setq ptLoc (calc-ptLoc curPt res))
            (setq dColor (if overrideColor overrideColor (get-dim-color curPt res)))

            ;; --- 線分作図 (0画層で描画) ---
            (setvar "CLAYER" "0")
            (command "_.LINE" "_non" curPt "_non" res "")
            (setq lineEnt (entlast))

            ;; --- 寸法作図 ---
            (check-layer)
            (command "_.DIMLINEAR" "_non" curPt "_non" res "_non" ptLoc)
            (setq dimEnt (entlast))
            (set-prop dimEnt "実測寸法" dColor)
            
            (setq history (cons (list curPt res lineEnt dimEnt) history))
            (setq curPt res)
          )
          (T (setq loop nil))
        )
      )
    )
  )

  (if oldLayer (setvar "CLAYER" oldLayer))
  (if oldDimlayer (setvar "DIMLAYER" oldDimlayer))
  (if oldDimexo (setvar "DIMEXO" oldDimexo))
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (princ "\nコマンドを終了します。")
  (princ)
)