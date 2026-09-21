(vl-load-com)

(defun c:SPLL ( / baseSS targetAreas tol ss i ent obj area matchSS count matchCount )
  (princ "\n=== 同面積スプライン複数選択コマンド (SPLL) ===")
  
  ;; 基準となる複数のスプラインを選択
  (prompt "\n基準となる閉じたスプラインを選択 (複数選択可): ")
  (setq baseSS (ssget '((0 . "SPLINE"))))
  
  (if baseSS
    (progn
      (setq targetAreas '())
      (setq tol 0.001) ; 許容誤差（±0.001）
      
      ;; 選択された基準スプラインから「閉じたもの」の面積リストを作成
      (repeat (setq i (sslength baseSS))
        (setq ent (ssname baseSS (setq i (1- i))))
        (setq obj (vlax-ename->vla-object ent))
        
        (if (and (vlax-property-available-p obj 'Closed)
                 (= (vla-get-closed obj) :vlax-true))
          (setq targetAreas (cons (vlax-curve-getArea obj) targetAreas))
        )
      )
      
      (if targetAreas
        (progn
          (princ (strcat "\n有効な基準面積の数: " (itoa (length targetAreas)) " 件"))
          
          ;; 図面内のすべてのスプラインを取得
          (setq ss (ssget "X" '((0 . "SPLINE"))))
          
          (if ss
            (progn
              (setq matchSS (ssadd))
              (setq matchCount 0)
              
              (repeat (setq i (sslength ss))
                (setq ent (ssname ss (setq i (1- i))))
                (setq obj (vlax-ename->vla-object ent))
                
                ;; 閉じていて、いずれかの基準面積と一致するか判定
                (if (and (vlax-property-available-p obj 'Closed)
                         (= (vla-get-closed obj) :vlax-true))
                  (progn
                    (setq area (vlax-curve-getArea obj))
                    
                    ;; 基準面積リストのいずれかと許容誤差内で一致するかチェック
                    (if (vl-some '(lambda (tar) (<= (abs (- area tar)) tol)) targetAreas)
                      (progn
                        (ssadd ent matchSS)
                        (setq matchCount (1+ matchCount))
                      )
                    )
                  )
                )
              )
              
              ;; 該当するオブジェクトを選択状態にする
              (if (> matchCount 0)
                (progn
                  (sssetfirst nil matchSS)
                  (princ (strcat "\n成功: 条件に一致するスプラインが " (itoa matchCount) " 個選択されました。"))
                )
                (princ "\n同じ面積のスプラインは見つかりませんでした。")
              )
            )
            (princ "\n図面内にスプラインが存在しません。")
          )
        )
        (princ "\nエラー: 選択されたスプラインの中に「閉じた」ものがありませんでした。")
      )
    )
    (princ "\nスプラインが選択されませんでした。")
  )
  (princ)
)