(defun c:BBC (/ *error* e p1 t1 p2 p_list ent_list last_e)
  ;; --- エラー・安全終了処理 ---
  (defun *error* (m)
    (setvar "CMDECHO" 1)
    (princ "\nBBC 終了")
    (princ)
  )
  
  (setvar "CMDECHO" 0)
  
  ;; 1. コピー元と基点の選択
  (if (and (setq e (car (entsel "\nコピー元の文字を選択: ")))
           (setq p1 (getpoint "\nコピーの基点を指定: ")))
    (progn
      (command "._UNDO" "_Begin")
      (setq ent_list nil
            p_list (list p1))
      
      (while t
        ;; 次の番号（またはアルファベット）を計算
        (setq t1 (inc (cdr (assoc 1 (entget e)))))
        (princ (strcat "\r>>> 配置: [" t1 "]  [左クリック]:確定  [右クリック]:ひとつ戻る  [Esc]:終了 "))
        
        ;; 2. 標準COPYコマンドでダイレクトにドラッグ配置
        (command "._copy" e "" "_none" (car p_list) pause)
        
        ;; 配置後の座標を取得
        (setq p2 (getvar "LASTPOINT"))
        
        ;; 3. 右クリック判定（座標が動いていなければ「ひとつ戻る」）
        (if (equal p2 (car p_list) 1e-6)
          (progn
            (if ent_list
              (progn
                (entdel (car ent_list))
                (setq ent_list (cdr ent_list)
                      p_list (cdr p_list)
                      e (if ent_list (car ent_list) (car (entsel "\n再選択してください: "))))
                (princ "\n>>> ひとつ戻りました")
              )
              (princ "\n>>> 戻る対象がありません")
            )
          )
          ;; 左クリックで正常に配置された場合
          (progn
            (setq last_e (entlast))
            
            ;; 配置された瞬間に文字を新しい連番に書き換え
            (entmod (subst (cons 1 t1) (assoc 1 (entget last_e)) (entget last_e)))
            (entupd last_e)
            
            ;; 履歴に追加
            (setq ent_list (cons last_e ent_list)
                  p_list (cons p2 p_list)
                  e last_e)
            
            (redraw)
          )
        )
      )
      (command "._UNDO" "_End")
    )
  )
  (setvar "CMDECHO" 1)
  (princ)
)

;; --- 連番・アルファベット自動判定関数 ---
(defun inc (s / i len last_char prefix num_str code)
  (setq len (strlen s))
  (if (= len 0)
    (setq s "A")
    (progn
      (setq last_char (substr s len 1))
      (cond
        ;; ① 末尾が数字の場合 (0～9)
        ((and (>= (ascii last_char) 48) (<= (ascii last_char) 57))
         (setq i len)
         (while (and (> i 0) (<= 48 (ascii (substr s i 1)) 57)) (setq i (1- i)))
         (setq prefix (substr s 1 i) num_str (substr s (1+ i)))
         (strcat prefix (itoa (1+ (atoi num_str))))
        )
        ;; ② 末尾が大文字アルファベットの場合 (A～Y)
        ((and (>= (ascii last_char) 65) (< (ascii last_char) 90))
         (setq prefix (substr s 1 (1- len))
               code (1+ (ascii last_char)))
         (strcat prefix (chr code))
        )
        ;; ③ 末尾が大文字の「Z」の場合 -> 「AA」へ繰り上げ
        ((= (ascii last_char) 90)
         (setq prefix (substr s 1 (1- len)))
         (strcat prefix "AA")
        )
        ;; ④ 末尾が小文字アルファベットの場合 (a～y)
        ((and (>= (ascii last_char) 97) (< (ascii last_char) 122))
         (setq prefix (substr s 1 (1- len))
               code (1+ (ascii last_char)))
         (strcat prefix (chr code))
        )
        ;; ⑤ 末尾が小文字の「z」の場合 -> 「aa」へ繰り上げ
        ((= (ascii last_char) 122)
         (setq prefix (substr s 1 (1- len)))
         (strcat prefix "aa")
        )
        ;; ⑥ その他（記号など）の場合は末尾に「1」を付加
        (t (strcat s "1"))
      )
    )
  )
)