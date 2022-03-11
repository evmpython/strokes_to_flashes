! OBJETIVO: Este programa agrupa strokes da brasildat para flashes

! DADOS DE ENTRADA: 
!   dados: strokes da brasildat da campanha CHUVA-VALE
!   formato: ASCII
!   nome: LDTL14100120111124000000.CSV.gz
!   exemplo: Time(UTC),Latitude,Longitude,Type(0=CG 1=IC),Peak Current(kA), Sensor Number
!            2011-11-24T00:00:16.595771372,-21.2215554,-42.4478449,0,-11.8,10
!            2011-11-24T00:00:16.623166441,-21.244236,-42.4311196,0,-10.9,9
!
! DADOS DE SAÍDA:
!   dados: flashes da brasildat da campanha CHUVA-VALE
!   formato: ASCII
!   nome: flashEVM_BRASILDAT_20111124.txt.gz
!   exemplo:    id    tipo  ano  mes dia hor min       seg          lat            lon         pc(A) alt_ic(m) sens mult_certa
!             99999999  CG  2011  11  24   0   0  16.595771372  -21.22155540  -42.44784490   -11.8        0    0         1
!             99999999  CG  2011  11  24   0   0  16.623166441  -21.24423600  -42.43111960   -10.9        0    0         2
!             99999999  CG  2011  11  24   0   0  16.640989691  -21.23014350  -42.44533070    -7.1        0    0         2

! PARA PROCESSAR: gfortran -o PRINCIPAL_evm_stroke_to_flash_brasildat_ATUAL PRINCIPAL_evm_stroke_to_flash_brasildat_ATUAL.f90 
!                 ./PRINCIPAL_evm_stroke_to_flash_brasildat_ATUAL

! REALIZADO POR: ENRIQUE V. MATTOS - 16/JAN/2019
! MODIFICADO POR: ENRIQUE V. MATTOS - 11/MAR/2022


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                           INICIO DO PROGRAMA
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

program strokes_to_flash

implicit none

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                           DECLARACAO DE VARIAVEIS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

integer	 	      nt
character*90 	  file_stroke
character*31    file_in
integer	 	      nl 
parameter	      (nl=500000)
integer		      id(nl)
character*2	    tipo(nl)
integer		      ano(nl)
integer         mes(nl)
integer         dia(nl)
integer         hor(nl)
integer         min(nl)
real*8		      seg(nl)
real*8		      lat(nl)
real*8          lon(nl)
real		        pc(nl)
integer         alt_ic(nl)
integer         sens(nl)
integer         Mult(nl)
real		        jultot(nl)
character*1000  lixo
real      	    dt,ds
integer   	    flag_stroke(nl)
real	  	      delta_lat
real	          delta_lon
real            distance
real        	  juli
integer	 	      count_stroke
integer		      count_total
integer         i
integer         j
integer         nstrokes
integer         multip
integer         i0
integer         i1
integer         ii
real		        dif_space
real            dif_time
character*90    file_flash
integer         cont
integer         total
integer         ierro
character*10000 lista
character*90    arquivo(10000)
character*10000 vetor(1000)
integer         narq
integer         k
character*100	  cmd
character*100   cabecalho

! A variavel status verifica se houve 
! algum erro ao abrir o arquivo
INTEGER :: status = 0

! Contador de posições de campos e número de campos
INTEGER :: posicao_campo = 0, campos = 0

! Aloca espaco para leitura de uma linha de texto
CHARACTER*2048 :: linha_texto

! Aloca espaço para um registro do banco de dados (78 campos)
CHARACTER*32, DIMENSION(78) :: dados

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                           LIMIAR ESPACIAL E TEMPORAL 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

dt = 0.50   !criterio de tempo:  intervalo de tempo maximo entre os strokes de um mesmo flash - 0.5 segundos
ds = 20.0   !criterio de espaco: distancia maxima entre os strokes de um mesmo flash - 20 km

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                              LER TODOS OS ARQUIVOS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! Exemplo do nome do arquivo: LDTL14100120120101000000.CSV
call system('ls -f '//'LDTL141001*.CSV.gz |cut -c 1-80 > lista_brasildat.txt')

open(1,file='lista_brasildat.txt',status="unknown")
cont=1
do 
   read (1, '(a)', end=93) arquivo(cont)
   arquivo(cont) = adjustl(arquivo(cont))
   cont=cont+1
enddo
93 continue
close(1)
narq = cont - 1

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                            NOME DO ARQUIVO DE STROKES
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

DO K=1,NARQ !LOOP DOS ARQUIVOS DE STROKES DA CLIMATEMPO     

  !=======================================
  !     NOME DO ARQUIVO DE STROKES
  !=======================================
  file_stroke = arquivo(k)
  print*,'PROCESSANDO ===>>>..... ',file_stroke

  !=======================================
  !LEH O ARQUIVO DE STROKES DA BRASILDAT
  !=======================================
  !EXEMPLO: 
  
  !LDTL14100120111101000000.CSV.gz
  
  !Time(UTC),Latitude,Longitude,Type(0=CG 1=IC),Peak Current(kA), Sensor Number
  !2012-01-01T00:00:04.035851895,-18.8458439,-50.0756166,1,5.2,7
  !2012-01-01T00:00:04.196217179,-18.8326594,-50.1124332,0,-41.1,10

  ! Abre o arquivo de leitura de raios da brasildat 
  call system('gunzip -f '//file_stroke)

  OPEN (UNIT=11, IOSTAT=status, FILE=file_stroke(01:28), STATUS='OLD')
	  
   	   read (11,*) cabecalho

           !=======================================================================
           ! Looping de leitura do arquivo anual de raios da CLIMATEMPO
           nt = 1
           
           DO  !*** LOOP DAS LINHAS DO ARQUIVO
        	! Lê uma linha completa (um registro)
        	READ(11, '(A)', IOSTAT=status) linha_texto

        	! Verifica se chegou no final do arquivo...
        	IF (status .LT. 0) THEN
            	   ! ...e sai do looping se finalizou
           	   EXIT
        	ENDIF

        	! Separa os campos utilizando o ',' como delimitador
        	posicao_campo = 1
       	 	campos = 1

        	DO i=1,len_trim(linha_texto) !*** LOOP DOS CARACTER DE UMA LINHA DO ARQUIVO
            	    ! se encontrar o ','...
            	    IF (linha_texto(i:i) == ',') THEN                
                
                        ! ...adiciona o campo no array 'dados'
                        dados(campos) = linha_texto(posicao_campo:i-1)
                        campos = campos + 1
                
                        ! marca a posição do último ',' encontrado
                        posicao_campo = i+1
            	    ENDIF       
        	ENDDO
        	dados(6) = linha_texto(len_trim(linha_texto):len_trim(linha_texto))
 
                !separa as variaveis
                id(nt) = 99999999
                if (dados(4) == '0') tipo(nt) = 'CG' 
                if (dados(4) == '1') tipo(nt) = 'IC' 
                read (dados(1)(1:4),'(i4)')      ano(nt) 
                read (dados(1)(6:7),'(i2)')      mes(nt) 
                read (dados(1)(9:10),'(i2)')     dia(nt) 
                read (dados(1)(12:13),'(i2)')    hor(nt) 
                read (dados(1)(15:16),'(i2)')    min(nt) 
                read (dados(1)(18:29),'(f12.9)') seg(nt) 
                read (dados(2),'(f11.7)')        lat(nt) 
                read (dados(3),'(f11.7)')        lon(nt) 
                read (dados(5),'(f6.1)')         pc(nt) 
                alt_ic(nt) = 0
                sens(nt)   = 0
                Mult(nt)   = 0
                jultot(nt) = (hor(nt)*3600.0) + (min(nt)*60.0) + seg(nt) 

 		!print*,linha_texto(1:len_trim(linha_texto))
	 	!print*,ano(nt) 
 		!print*,mes(nt) 
 		!print*,dia(nt) 
 		!print*,hor(nt) 
 		!print*,min(nt) 
 		!print*,seg(nt) 
 		!print*,lat(nt) 
 		!print*,lon(nt) 
 		!print*,tipo(nt) 
 		!print*,pc(nt)
 		!print*,jultot(nt)
 		!stop

           	nt = nt + 1

           END DO ! FIM DO Looping de leitura do arquivo de raios da brasildat 
         !=======================================================================
  call system('gzip -f '//file_stroke(01:28))
  close(11)
  nt = nt -1 
  
  !================================
  !   JUNTA STROKES EM FLASHES
  !================================
  nstrokes     = nt    !numero de strokes do arquivo

  flag_stroke  = 1     !(nl)-vetor indica a qual flash o stroke pertence
  count_stroke = 1     !numero sequencial dos flashes encontrados 
  count_total  = 0     !total de strokes
  i            = 1     !contador do stroke atual

  DO WHILE (i < nstrokes) !o stroke atual eh ateh o nstrokes-1
  
  if (flag_stroke(i) == 1) then           
    
    flag_stroke(i) = count_stroke
    multip         = 1
    
    if (i < nstrokes) then  

      i0   = i + 1
      i1   = nstrokes

      juli = (hor(i)*3600.0) + (min(i)*60.0) + seg(i)        !tempo em segundos do dia
                
      !VARRE DA LINHA I+1 ATEH A ULTIMA LINHA DO ARQUIVO
      !============================================================================
      do ii=i0,i1 
        delta_lat = lat(i)-lat(ii)
        delta_lon = lon(i)-lon(ii)
        distance  = ( SQRT ((delta_lat)**2 + (delta_lon)**2) )*111.195 !em km
           
        dif_space = distance               !distancia entre os strokes
        dif_time  = jultot(ii)-juli        !tempo entre os strokes

       if (dif_time <= dt .and. dif_space <= ds) then  
          flag_stroke(ii) = count_stroke
          multip          = multip + 1
        endif
      enddo
      !============================================================================

    endif
    count_stroke = count_stroke + 1
    count_total  = count_total  + multip
  endif
  i = i + 1
  ENDDO

  !================================
  ! IMPRIME ARQUIVO DE FLASHES
  !================================
  file_flash = 'flashEVM_BRASILDAT_'//file_stroke(11:18)//'.txt'
    
  
  open(2,file=file_flash,status='unknown')
  write(2,'(a)')'      id  tipo  ano  mes dia hor min       seg          lat            lon         pc(A) alt_ic(m) sens mult_certa'
  do j=1,nt 
     
     if (tipo(j) == 'IC') then
       flag_stroke(j) = 0 
     endif

     write(2,'(i10, 2x, a2, 2x, i4, 2x, i2, 2x, i2 ,2x, i2, 2x, i2, &
     &2x, f12.9, 2x, f12.8,&     
     &2x,f12.8, 2x, f6.1, 3x, i6, 3x, i2, 3x, i7)')&
     &id(j), tipo(j), int(ano(j)), int(mes(j)),&     
     &int(dia(j)), int(hor(j)), int(min(j)), seg(j), lat(j), lon(j),&
     &pc(j), alt_ic(j), sens(j),&
     &flag_stroke(j) 

  enddo
  close(2)
  call system('gzip -f '//file_flash)

!print*,'teste'
!stop

ENDDO !fim do loop dos arquivos

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                          FIM DO PROGRAMA
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

print*,'total_de_strokes=',count_total
print*,'total_de_flashes=',maxval(flag_stroke)
print*,'fim do programa'

end