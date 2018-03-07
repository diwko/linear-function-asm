dane segment
    
    napis_start         db  "Program rysujacy wykres funkcji liniowej oraz obliczajacy jej miejsce zerowe.",10,10,13,13,'$'    
    napis_parametry     db  "Podaj wspolczynniki a i b funkcji liniwej y=a*x+b (zakres [-327;327]):$"
    napis_a             db  "a = $"
    napis_b             db  "b = $"
    napis_e1            db  "Niepoprawne dane!$"
    napis_brak_rozw     db  "Brak miejsca zerowego.$" 
    napis_nieskoncz     db  "Nieskonczenie wiele miejsc zerowych$"
    napis_przecinek     db  ",$"
    newline             db  13,10,'$'
    napis_rozwiazanie   db  "Miejsce zerowe: x = $"
    napis_skala         db  "Skala: $" 
    napis_koniec1       db  "ESC - WYJDZ$"
    napis_koniec2       db  "INNY KLAWISZ - POWTORZ$"    
         
    par_a_string        db  20,0,22 dup('$')     
    par_a               dw  0
    par_b_string        db  20,0,22 dup('$')
    par_b               dw  0
    znak                dw  1                   ;1 dodatnia,    -1  ujemna
    ulamek              dw  10                  ;1 ulamek       10  calkowita
    zaokraglanie        dw  0 
    
    rozwiazanie         dw  0
    znak_rozwiazanie    dw  1
    
    wykres              dw  441 dup('.') 
    skala               dw  10
   
    
    
dane ends

code segment

start:
                mov		ax,seg w_stosu			    ;ustawienie stosu
                mov		ss,ax
                mov     sp,offset w_stosu  
				
                mov     ax,seg znak                 ;ustawienie segmentu danych                     
                mov     ds,ax                                               
			    
                call    clear_screen
			    
                mov     dx,offset [napis_start]
                call    wypisz_tekst 
                
    wczytywanie_danych:     
    
                mov     dx,offset [napis_parametry]
                call    wypisz_tekst
                call    wypisz_newline
               
                mov     dx,offset [napis_a]
                call    wypisz_tekst
                mov     dx,offset [par_a_string]      ;wczytaj a
                call    wczytaj_tekst  
                call    wypisz_newline
                
                mov     bx,offset [par_a_string]      ;zamien a na int
                call    string_to_int         
                mov     word ptr [par_a],bx
                
                mov     dx,offset [napis_b]           ;wczytaj b
                call    wypisz_tekst
                mov     dx,offset [par_b_string]
                call    wczytaj_tekst          			
                call    wypisz_newline
                
                mov     bx,offset [par_b_string]      ;zamien b na int
                call    string_to_int         
                mov     word ptr [par_b],bx
                
                                
                call    clear_screen
                call    msc_zerowe 
                call    oblicz_skala
			   
				call    pokaz_napisy
					     
				call    zaznacz_osie 			    
				call    zaznacz_punkty
					    
				mov     dh,0
		        mov     dl,0
		        call    ustaw_kursor
			    			    
				call    wyswietl_wykres
			    
                mov     ah,8                        ;czekaj na klawisz
                int     21h
                cmp     al,27                       ;27 = esc
                je      wyjscie_z_programu
                
                call    resetowanie_danych
                            
                call    clear_screen
                
                jmp     wczytywanie_danych 
                
				
		wyjscie_z_programu:    		

				mov	    ax,04c00h  		    ;koniec programu i powrot do DOS
				int 	21h


;................................  
;in 
;out console
          
wypisz_newline: 
                push    ax
                push    dx			
				
				mov     dx,offset [newline]
				mov     ah,09h
				int     21h           
						
				pop     dx
				pop     ax
				ret
;...............................
;in dx
;out console

wypisz_tekst:   
                push    ax     
                mov     ah,09h
				int     21h           
				pop     ax
				ret
;................................
;in console
;out dx
                             
wczytaj_tekst:  
                push    ax                
                mov     ah,0ah
                int     21h
                pop     ax
                ret 
;................................
;in dh-wiersz,dl-kolumna 

ustaw_kursor:
                push    ax
                push    bx
                xor     bx,bx
                mov     ah,2
                int     10h
                pop     bx                
                pop     ax
                ret
;................................
clear_screen:
                push    ax
                mov     ah,00h                      ;czysci ekran
                mov     al,03                   
                int     10h
                pop     ax
                ret                
                  
;................................
;in bx (string) 
;out bx (int)
 
string_to_int:            
                push    ax                          ;wynik
                push    cx                          ;licznik    
                push    dx                          ;pojedynczy znak
                push    si                          ;licznik
                
                mov     ax,1
                mov     word ptr [znak],ax 
                mov     ax,10
                mov     word ptr [ulamek],ax
                
                xor     cx,cx         
                mov     cl,byte ptr [bx+1]          ;cl liczba znakow                
                                               
                xor     ax,ax
                xor     dx,dx                
                xor     si,si 
                
        poczatek_znaki:
                mov     dl,byte ptr [bx + 2 + si]   ;dl pojedynczy znak
                
                cmp     dl,'-'                      
                je      zmien_znak0
                
                cmp     dl,'+'
                je      zmien_znak1
                
                cmp     dl,' '
                jne     p1
                inc     si
                dec     cx
                jmp     poczatek_znaki
                      
        p1:                          
        
                mov     dl,byte ptr [bx + 2 + si]   ;dl pojedynczy znak
                
                cmp     dl,'.'
                je      przecinek
                
                cmp     dl,','
                je      przecinek
                
                cmp     dl,' '
                je      pomin
                
                cmp     dl,'0'
                jb      error1
                cmp     dl,'9'
                ja      error1
                                
                sub     dl,'0'                      ;na int
                
                push    cx
                push    dx
                xor     dx,dx
                mov     cx,10
                mul     cx                          ;ax=ax*10
                pop     dx
                pop     cx
                              
                add     ax,dx                       ;ax=ax+dx
           
            pomin:
               
                inc     si                               
                loop    p1
                
                add     ax,word ptr [zaokraglanie]
                mul     ulamek                            
                 
                mov     cx,word ptr [znak]
                imul    cx  
                
                cmp     ax,3270
                jg      error1
                
                cmp     ax,-3270
                jl      error1
                               
                mov     bx,ax
		mov 	word ptr [zaokraglanie],0                
     
                pop     si
                pop     dx
                pop     cx
                pop     ax  
                
                ret            
;...............................

error1:         
                push    dx
                
                call    wypisz_newline
                
                mov     dx,offset [napis_e1]
                call    wypisz_tekst
                call    wypisz_newline 
                
                pop     dx
                
                jmp     wczytywanie_danych
                
;.................................

zmien_znak0: 
                push    ax
                mov     ax,-1
                mov     word ptr [znak],ax                
                pop     ax
                jmp     pomin

;..................................
                
zmien_znak1:
                push    ax 
                mov     al,-1
                mov     byte ptr [znak],al
                pop     ax                  
                jmp     pomin   
                
;..................................

przecinek:      
                cmp     cl,1
                je      pomin
                
                push    ax
                mov     ax,1
                mov     byte ptr [ulamek],al
                pop     ax
                
                cmp     cl,2
                je      pomin  
                              
                mov     cl,2
                cmp     byte ptr [bx + 2 + si + 2],'5'                
                jb      pomin          
                
                push    ax
                mov     ax,1
                mov     word ptr [zaokraglanie],ax  
                pop     ax
                jmp     pomin
                
;.....................................
;in
;out [rozwiazanie]

msc_zerowe:
                push    ax
                push    bx
                push    dx 
                
                xor     ax,ax  
                xor     dx,dx
                
                
                        
                mov     bx,word ptr [par_a]                ;bx=a
                mov     ax,word ptr [par_b]                ;ax=b
                
                cmp     bx,0
                jne     a_rozne_0
                
                cmp     ax,0
                jne     b_rozne_0 
                
                mov     dh,5
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_nieskoncz]
                call    wypisz_tekst
                
                jmp     brak_rozw  
                
            b_rozne_0:
                
                mov     dh,5
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_brak_rozw]
                call    wypisz_tekst
                
                jmp     brak_rozw
            
            
            a_rozne_0:
                    
                call    okresl_znak_rozw  
            
                div     bx
                
                
                push    dx
                
                push    bx
                mov     bx,10                
                mul     bx                                 ;ax=ax*10
                pop     bx
                
                pop     dx
                push    ax                                 ;ax=reszta z dzielenia
                mov     ax,dx
                
                xor     dx,dx
                push    bx
                mov     bx,10                
                mul     bx
                pop     bx
                 
                div     bx                                 ;ax=1 cyfra po przecinku
                
                push    ax
                
                                
                mov     ax,dx                              ;ax=reszta
                xor     dx,dx
                push    bx
                mov     bx,10                
                mul     bx
                pop     bx
                div     bx                                 ;ax=2 cyfra po przecinku
                
                pop     bx                                 ;bx=1 cyfra po przecinku
                pop     dx                                 ;dx= calosc
                add     dx,bx
                
                cmp     ax,5                               
                jb      koniec_msc_zerowe
                
                inc     dx   
               
      koniec_msc_zerowe: 
                
                mov     ax,dx
                imul    [znak_rozwiazanie]
                neg     ax                                                
                mov     word ptr [rozwiazanie],ax   
               
            brak_rozw:
                              
                pop     dx 
                pop     bx
                pop     ax
                                
                ret 
                
;..........................................
;in ax,bx
;out jesli ujemna zmienia [znak_rozwiazanie] na -1

okresl_znak_rozw:
                push    dx
                
                cmp     ax,0
                jl      ujemna1
                
                cmp     bx,0
                jge     znak_plus
                jmp     znak_minus
                
           ujemna1:
                neg     ax                 
                cmp     bx,0
                jl      znak_plus                  
           znak_minus:                
                mov     dx,-1               
                mov     word ptr [znak_rozwiazanie],dx  
           znak_plus:
                call    neg_bx           
                pop     dx
                ret
                
     neg_bx:    
                cmp     bx,0
                jge     bx_dodatnie
                neg     bx
            bx_dodatnie:  
                ret
                      
                 
                
;..........................................
;in ax
;out console

wypisz_liczbe:                                              ;zamienia na ascii i wypisuje
                                                            ;dzieli przez 10, odklada na stos
                push     bx                                  ;sciaga ze stosu, wypisuje
                push     cx
                push     dx
                             
                xor      cx,cx 
               
                mov      bx,10
               
                cmp      ax,0
                jl       wypisz_minus
     
        dziel10:
                xor      dx,dx    
                div      bx
                push     dx
                inc      cx
               
                cmp      ax,0
                jnz      dziel10
               
                dec      cx
               
                cmp      cx,0
                je       wypisz_przecinek 
               
        wypisz_cyfry:
        
                pop     dx
                add     dx,'0'
                mov     ah,2
                int     21h
                
                loop    wypisz_cyfry 
                
                mov     dx,offset [napis_przecinek]
                call    wypisz_tekst
                
                pop     dx  
                add     dx,'0'
                mov     ah,2
                int     21h
         
        zakonczenie_proc: 
                
                pop     dx
                pop     cx
                pop     bx
                
                ret 
                
        wypisz_minus:
                
                push    dx
                mov     dx,-1
                imul    dx
                
                mov     dx,'-'
                push    ax
                
                mov     ah,2
                int     21h                
                
                pop     ax                
                pop     dx         
               
                jmp     dziel10 
                
        wypisz_przecinek:
         
                mov     dx,0
                add     dx,'0'
                mov     ah,2
                int     21h                
                
                mov     dx,offset [napis_przecinek]
                call    wypisz_tekst
                
                pop     dx  
                add     dx,'0'
                mov     ah,2
                int     21h
                
                jmp     zakonczenie_proc 
                
;...................................		
wyswietl_wykres: 
                push    ax
                push    bx
                push    cx
                push    dx
                push    si
                
                mov     cx,441
                mov     si,0
                xor     dx,dx  
                mov     bx,21
                
          wypisuj:
                
                xor     dx,dx
                mov     ax,cx
                div     bx
                cmp     dx,0
                jne     pomin_nowa_linia
                call    wypisz_newline
          
          pomin_nowa_linia:
          
                mov     dx,word ptr [wykres+si]
                mov     ah,2
                int     21h
                inc     si
                inc     si

                loop    wypisuj
                
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                
                ret
                
;.................................  
zaznacz_osie:    
                push    ax
                push    cx
                push    si
                
                 
                mov     cx,21
                mov     si,0
                mov     ax,'#'
        osx:    
                mov     word ptr [wykres+420+si],ax
                inc     si
                inc     si
                loop    osx 
            
                mov     cx,21
                mov     si,0   
        osy:
                mov     word ptr [wykres+20+si],ax
                add     si,42
                loop    osy               
                
                pop     si
                pop     cx
                pop     ax                               
                ret                  
;.................................
                                  
oblicz_skala:                                                 ;ustawia skale w zaleznosci od
                push    ax                                    ;miejsca zerowego i parametru b
                push    bx
                push    dx
                
                mov     ax,word ptr [rozwiazanie]
                mov     bx,word ptr [par_b]
                
                cmp     ax,0
                jge     dodatnia1
                neg     ax
            dodatnia1:
                cmp     bx,0
                jge     dodatnia2
                neg     bx
            dodatnia2:
                
                cmp     ax,bx
                ja      x_wiekszy
                
                push    ax
                mov     ax,bx
                mov     bx,5
                xor     dx,dx
                div     bx
                mov     word ptr [skala],ax
                pop     ax
                
                jmp     skala_ustawiona
                
            x_wiekszy:
                mov     bx,5
                xor     dx,dx
                div     bx
                mov     word ptr [skala],ax        
                       
                
            skala_ustawiona:    
                
                pop     dx
                pop     bx
                pop     ax
                ret
                
;.................................
zaznacz_punkty:
                push    ax
                push    bx
                push    cx
                push    dx
                push    si 
                
                mov     ax,word ptr [skala]                    ;skala/10
                mov     bx,10
                xor     dx,dx
                div     bx
                cmp     ax,0
                jne     hop
                inc     ax
            hop:    
                mov     word ptr [skala],ax
                
                mov     dh,6                                     ;wypisz skala
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_skala]
                call    wypisz_tekst
                
                xor     dx,dx
                mov     bx,10
                mul     bx
                call    wypisz_liczbe
                          
                mov     bx,word ptr [par_a]                    ;skoki w zaleznosci od par_a
                cmp     bx,0
                je      funkcja_stala
                
                cmp     bx,10
                jge     zaznacz_wzgl_y
                
                cmp     bx,-10
                jle     zaznacz_wzgl_y       
                
                
    zaznacz_wzgl_x:
                
                mov     cx,21                                  ;cx licznik petli
                mov     si,-10                                 ;si  wartosc x bez skali
                
        punkty_dla_x:
                mov     bx,word ptr [par_a]
                mov     ax,word ptr [skala]
                xor     dx,dx
                imul    si                                     ;ax=ax/si     y=ax+b
                imul    bx                                                   ;a=bx
                add     ax,[par_b]                                           ;x=si*bx(skala)
                
                xor     dx,dx
                cmp     ax,0
                jge     x_wiekszy_od_zera
                mov     dx,-1
            x_wiekszy_od_zera:
                mov     bx,word ptr [skala]
                idiv    bx
                xor     dx,dx 
                cmp     ax,0
                jge     x_wiekszy_od_zera2
                mov     dx,-1
            x_wiekszy_od_zera2:
                mov     bx,10                                    ;si=x
                idiv    bx                                       ;ax=y
               
                cmp     ax,10
                jg      nieprawidlowy_y
                cmp     ax,-10
                jl      nieprawidlowy_y
                
                mov     bx,-42                                   ;adres punktu=2x-42y+440
                xor     dx,dx
                imul    bx
                add     ax,si
                add     ax,si
                push    si
                mov     si,ax
                mov     ax,'*'
                mov     word ptr [wykres+si+440],ax
                pop     si
               
            nieprawidlowy_y:
                
                inc     si
                
                loop    punkty_dla_x
                
                jmp     koniec_zaznacz_punkty
                
                             
    zaznacz_wzgl_y:
    
                mov     cx,21                                  ;cx licznik petli
                mov     si,-10                                 ;si  wartosc x bez skali            
                
        punkty_dla_y:
        
                mov     ax,word ptr [skala]
                xor     dx,dx
                mov     bx,10
                imul    bx
                
                mov     bx,word ptr [par_b]
                
                xor     dx,dx
                imul    si
                sub     ax,bx
                
                mov     bx,word ptr [par_a]
                xor     dx,dx
                cmp     ax,0
                jge     y_wiekszy_od_zera
                mov     dx,-1
            y_wiekszy_od_zera:
                idiv    bx                                      
                
                mov     bx,word ptr [skala]
                xor     dx,dx
                cmp     ax,0
                jge     y_wiekszy_od_zera2
                mov     dx,-1
            y_wiekszy_od_zera2:
                idiv    bx                                      ;ax=x
                
                cmp     ax,10
                jg      nieprawidlowy_x
                cmp     ax,-10
                jl      nieprawidlowy_x
                
                add     ax,ax                                   ;adres punktu=2x-42y+440
                
                push    ax
                mov     ax,si
                mov     bx,-42
                xor     dx,dx
                imul    bx
                pop     bx
                          
                add     ax,bx
                
                push    si
                mov     si,ax
                mov     ax,'*'
                mov     word ptr [wykres+si+440],ax
                pop     si
                
            nieprawidlowy_x:
                
                inc     si
                              
                loop    punkty_dla_y
                
                jmp     koniec_zaznacz_punkty
                
    funkcja_stala:
                
                mov     ax,word ptr [par_b]
                mov     bx,word ptr [skala]
                
                xor     dx,dx
                cmp     ax,0
                jge     b_wieksze_od_zera1
                mov     dx,-1
          b_wieksze_od_zera1:
                idiv    bx
                
                mov     bx,10
                xor     dx,dx
                cmp     ax,0
                jge     b_wieksze_od_zera2
                mov     dx,-1
          b_wieksze_od_zera2:                
                idiv    bx                                      ;ax=y
                
                mov     cx,21
                mov     si,-10 
                
                mov     bx,-42
                xor     dx,dx
                imul    bx 
                add     ax,si
                add     ax,si 
                
                mov     si,ax
                mov     bx,'*'
                
            punkty_dla_stala:
                                
                mov     word ptr [wykres+si+440],bx 
                inc     si
                inc     si
                
                loop    punkty_dla_stala 
                        
        
    koniec_zaznacz_punkty:
            
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                
                ret                
                               
;.................................
resetowanie_danych:

                push    ax
                push    cx
                push    si
                
                mov     ax,0
                mov     byte ptr [par_a_string+1],al
                mov     byte ptr [par_b_string+1],al    
                mov     word ptr [par_a],ax 
                mov     word ptr [par_b],ax 
                mov     word ptr [zaokraglanie],ax 
                mov     word ptr [rozwiazanie],ax    
                mov     ax,1
                mov     word ptr [znak],ax
                mov     word ptr [znak_rozwiazanie],ax  
                mov     ax,10
                mov     word ptr [skala],ax
                mov     word ptr [ulamek],ax
                
                mov     si,0
                mov     al,'$'
        wyzeruj_string:
                
                mov     byte ptr [par_a_string+2+si],al 
                mov     byte ptr [par_b_string+2+si],al
                
                inc     si                
                cmp     si,12
                jb      wyzeruj_string
                
                mov     si,0
                mov     ax,'.'
        wyzeruj_wykres:
                
                mov     word ptr [wykres+si],ax
                
                inc     si
                inc     si         
                cmp     si,882
                jb      wyzeruj_wykres 
                
                pop     si
                pop     cx
                pop     ax
                ret
;.................................

pokaz_napisy:
                push    ax
                push    dx 
                
                mov     dh,2
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_a]
                call    wypisz_tekst
                mov     ax,word ptr [par_a]
                call    wypisz_liczbe
                
                
                mov     dh,3
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_b]
                call    wypisz_tekst
                mov     ax,word ptr [par_b]
                call    wypisz_liczbe
                
                
                mov     dh,5
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,word ptr [par_a]
                cmp     dx,0
                je      pomin_rozw
                     
                mov     dx,offset [napis_rozwiazanie]
                call    wypisz_tekst
                
                mov     ax,word ptr [rozwiazanie]
                call    wypisz_liczbe
                call    wypisz_newline
                
        pomin_rozw:               
                
                mov     dh,9
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_koniec1]
                call    wypisz_tekst
                
                
                mov     dh,10
                mov     dl,30
                call    ustaw_kursor
                
                mov     dx,offset [napis_koniec2]
                call    wypisz_tekst
                
                pop     dx
                pop     ax
                ret
;................................. 

code ends
                 
                 
stos segment stack

				dw	    200 dup(?)			;rezerwacje 0-399 bitow
	w_stosu		dw 	    ?					;rezerwacja 400 bitu na wierzcholek

stos ends    
             
end start