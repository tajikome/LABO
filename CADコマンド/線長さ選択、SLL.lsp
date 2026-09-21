(defun c:SLL ( / refSs targetList i ent obj len lay tolInput searchSs newSs objType refLen refLay matchFound)
  (vl-load-com)
  
  ;; 前回入力した許容誤差の保持（初期値は0.001）
  (if (not *globalTol*)
    (setq *globalTol* 0.001)
  )
  
  ;; 1. 基準となる線を複数選択
  (princ "\n基準となる線を複数選択してください（選択後Enterで確定）:")
  (setq refSs (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  
  (if refSs
    (progn
      (setq targetList '())
      (setq i 0)
      
      ;; 基準線の「長さ」と「レイヤー」のペアをリスト化
      (repeat (sslength refSs)
        (setq ent (ssname refSs i))
        (setq obj (vlax-ename->vla-object ent))
        (if (vlax-property-available-p obj 'Length)
          (progn
            (setq len (vla-get-Length obj))
            (setq lay (vla-get-Layer obj))
            ;; ( (長さ . レイヤー名) ... ) の形式でリストに追加
            (setq targetList (cons (cons len lay) targetList))
          )
        )
        (setq i (1+ i))
      )
      
      (if targetList
        (progn
          (princ (strcat "\n基準線 " (itoa (length targetList)) " 本の条件を取得しました。"))
          
          ;; 2. 許容誤差の入力 (Enterで前回値を採用)
          (setq tolInput (getreal (strcat "\n許容誤差を入力 <" (rtos *globalTol* 2 4) ">: ")))
          (if tolInput
            (setq *globalTol* tolInput)
          )
          
          ;; 3. 図面全体からすべてのオブジェクトを取得
          (setq searchSs (ssget "X"))
          
          (if searchSs
            (progn
              (setq newSs (ssadd))
              (setq i 0)
              
              ;; 4. 図面内のオブジェクトを1つずつ判定
              (repeat (sslength searchSs)
                (setq ent (ssname searchSs i))
                (setq obj (vlax-ename->vla-object ent))
                (setq objType (vla-get-ObjectName obj))
                
                ;; 線オブジェクトかつ長さプロパティを保持しているか確認
                (if (and (member objType '("AcDbLine" "AcDbPolyline" "AcDb2dPolyline" "AcDb3dPolyline"))
                         (vlax-property-available-p obj 'Length))
                  (progn
                    (setq len (vla-get-Length obj))
                    (setq lay (vla-get-Layer obj))
                    (setq matchFound nil)
                    
                    ;; 登録した基準線条件のいずれかに一致するか判定
                    (foreach item targetList
                      (setq refLen (car item))
                      (setq refLay (cdr item))
                      
                      (if (and (= (strcase lay) (strcase refLay))
                               (<= (abs (- len refLen)) *globalTol*))
                        (setq matchFound t)
                      )
                    )
                    
                    ;; 条件に一致したら選択セットに追加
                    (if matchFound
                      (ssadd ent newSs)
                    )
                  )
                )
                (setq i (1+ i))
              )
              
              ;; 5. 一致した要素をハイライト表示
              (if (> (sslength newSs) 0)
                (progn
                  (sssetfirst nil newSs)
                  (princ (strcat "\n【図面全体】条件に一致する線を " (itoa (sslength newSs)) " 本選択しました。"))
                )
                (princ "\n図面内に条件に一致する線は見つかりませんでした。")
              )
            )
            (princ "\n図面内に要素が存在しません。")
          )
        )
      )
    )
    (princ "\n基準線が選択されませんでした。")
  )
  (princ)
)