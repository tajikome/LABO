(defun c:KH ( / type_choice lower upper has_ueoki ueoki has_kasagi kasagi ss ent eg txt sel hist done h)
  ;; 1. 最初に作成タイプを選択（初期値はN:通常）
  (initget "N T M")
  (setq type_choice (getkword "\nタイプを選択 [通常(N)/天板付(T)/モジュラス(M)] : "))
  (if (or (= type_choice "") (not type_choice))
    (setq type_choice "N")
  )

  ;; 2. 選択によって入力を分岐
  (cond
    ;; ====================================
    ;; 【T: 天板付】
    ;; ====================================
    ((= type_choice "T")
      ;; 下の数字（1回のみ）を取得
      (if *last_sst_lower*
        (setq lower (getstring (strcat "\n数字を入力 <" *last_sst_lower* ">: ")))
        (setq lower (getstring "\n数字を入力: "))
      )
      (if (= lower "") (setq lower *last_sst_lower*) (setq *last_sst_lower* lower))
      
      ;; H自動付与
      (if (and lower (/= lower "") (not (wcmatch (strcase lower) "*H"))) (setq lower (strcat lower "H")))
      
      ;; テキスト生成
      (setq txt (strcat "天板付:" lower))
    )

    ;; ====================================
    ;; 【N: 通常】
    ;; ====================================
    ((= type_choice "N")
      ;; 下の数字
      (if *last_sst_lower*
        (setq lower (getstring (strcat "\n下の数字を入力 <" *last_sst_lower* ">: ")))
        (setq lower (getstring "\n下の数字を入力: "))
      )
      (if (= lower "") (setq lower *last_sst_lower*) (setq *last_sst_lower* lower))
      (if (and lower (/= lower "") (not (wcmatch (strcase lower) "*H"))) (setq lower (strcat lower "H")))

      ;; 上の数字
      (if *last_sst_upper*
        (setq upper (getstring (strcat "\n上の数字を入力 <" *last_sst_upper* ">: ")))
        (setq upper (getstring "\n上の数字を入力: "))
      )
      (if (= upper "") (setq upper *last_sst_upper*) (setq *last_sst_upper* upper))
      (if (and upper (/= upper "") (not (wcmatch (strcase upper) "*H"))) (setq upper (strcat upper "H")))

      ;; テキスト生成
      (setq txt (strcat "下:" lower "／上:" upper))
    )

    ;; ====================================
    ;; 【M: モジュラス】
    ;; ====================================
    ((= type_choice "M")
      ;; 下の数字
      (if *last_sst_lower*
        (setq lower (getstring (strcat "\n下の数字を入力 <" *last_sst_lower* ">: ")))
        (setq lower (getstring "\n下の数字を入力: "))
      )
      (if (= lower "") (setq lower *last_sst_lower*) (setq *last_sst_lower* lower))
      (if (and lower (/= lower "") (not (wcmatch (strcase lower) "*H"))) (setq lower (strcat lower "H")))

      ;; 上の数字
      (if *last_sst_upper*
        (setq upper (getstring (strcat "\n上の数字を入力 <" *last_sst_upper* ">: ")))
        (setq upper (getstring "\n上の数字を入力: "))
      )
      (if (= upper "") (setq upper *last_sst_upper*) (setq *last_sst_upper* upper))
      (if (and upper (/= upper "") (not (wcmatch (strcase upper) "*H"))) (setq upper (strcat upper "H")))

      ;; 上置あり？（1 を入力すると数字入力へ）
      (initget "1 N")
      (setq has_ueoki (getkword "\n上置あり？ [1/N] : "))
      (if (or (= has_ueoki "") (not has_ueoki)) (setq has_ueoki "N"))
      (if (= has_ueoki "1")
        (progn
          (if *last_sst_ueoki*
            (setq ueoki (getstring (strcat "\n上置の数字を入力 <" *last_sst_ueoki* ">: ")))
            (setq ueoki (getstring "\n上置の数字を入力: "))
          )
          (if (= ueoki "") (setq ueoki *last_sst_ueoki*) (setq *last_sst_ueoki* ueoki))
          (if (and ueoki (/= ueoki "") (not (wcmatch (strcase ueoki) "*H"))) (setq ueoki (strcat ueoki "H")))
        )
      )

      ;; 笠木あり？（1 を入力すると数字入力へ）
      (initget "1 N")
      (setq has_kasagi (getkword "\n笠木あり？ [1/N] : "))
      (if (or (= has_kasagi "") (not has_kasagi)) (setq has_kasagi "N"))
      (if (= has_kasagi "1")
        (progn
          (if *last_sst_kasagi*
            (setq kasagi (getstring (strcat "\n笠木の数字を入力 <" *last_sst_kasagi* ">: ")))
            (setq kasagi (getstring "\n笠木の数字を入力: "))
          )
          (if (= kasagi "") (setq kasagi *last_sst_kasagi*) (setq *last_sst_kasagi* kasagi))
          (if (and kasagi (/= kasagi "") (not (wcmatch (strcase kasagi) "*H"))) (setq kasagi (strcat kasagi "H")))
        )
      )

      ;; テキスト生成（モジュラスベース）
      (setq txt (strcat "モジュラス／下:" lower "／上:" upper))
      ;; 上置がある場合は結合
      (if (= has_ueoki "1")
        (setq txt (strcat txt "／上置:" ueoki))
      )
      ;; 笠木がある場合は結合
      (if (= has_kasagi "1")
        (setq txt (strcat txt "／笠木:" kasagi))
      )
    )
  )

  ;; 3. テキストを連続で置換（U:1つ戻す / Enter・Escで終了）
  (if txt
    (progn
      (princ (strcat "\n設定値: " txt))
      (setq hist nil done nil)
      (while (not done)
        (initget "U")
        (setvar "ERRNO" 0)
        (setq sel (entsel "\n置き換えるテキストをクリック [戻す(U)/Enterで終了]: "))
        (cond
          ;; U: 直前の置換を元に戻す
          ((= (type sel) 'STR)
            (if hist
              (progn
                (setq h (car hist) hist (cdr hist))
                (entmod (cdr h))
                (entupd (car h))
                (princ (strcat "\n1つ戻しました。（残り履歴: " (itoa (length hist)) "）"))
              )
              (princ "\n戻す履歴がありません。")
            )
          )
          ;; テキストをクリック
          ((and sel (listp sel))
            (setq ent (car sel))
            (setq eg (entget ent))
            (if (member (cdr (assoc 0 eg)) '("TEXT" "MTEXT"))
              (progn
                (setq hist (cons (cons ent eg) hist))
                (entmod (subst (cons 1 txt) (assoc 1 eg) eg))
                (entupd ent)
                (princ "\n置換しました。")
              )
              (princ "\nテキストではありません。")
            )
          )
          ;; 何もない場所をクリック → やり直し
          ((= (getvar "ERRNO") 7)
            (princ "\n何もない場所です。もう一度クリックしてください。")
          )
          ;; Enter/Esc → 終了
          (t (setq done T))
        )
      )
    )
  )
  (princ)
)