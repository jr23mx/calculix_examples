!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998-2023 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!
      subroutine dflux(flux,sol,kstep,kinc,time,noel,npt,coords,
     &     jltyp,temp,press,loadtype,area,vold,co,lakonl,konl,
     &     ipompc,nodempc,coefmpc,nmpc,ikmpc,ilmpc,iscale,mi,
     &     sti,xstateini,xstate,nstate_,dtime)
!
!     user subroutine dflux
!
      implicit none
!
      character*8 lakonl
      character*20 loadtype
!
      integer kstep,kinc,noel,npt,jltyp,konl(20),nstate_,
     &  nmpc,ikmpc(*),ilmpc(*),iscale,mi(*),
     &  ipompc(*),nodempc(3,*)
!
      real*8 flux(2),time(2),coords(3),sol,temp,press,area,
     &  coefmpc(*),sti(6,mi(1),*),xstate(nstate_,mi(1),*),
     &  xstateini(nstate_,mi(1),*),dtime,
     &  vold(0:mi(2),*),co(3,*)

! ========== PARÁMETROS DE CONFIGURACIÓN ==========
      integer max_points
      parameter (max_points = 100)
      real*8 tolerance
      parameter (tolerance = 1.0d-10)

! ========== VARIABLES PARA TRAYECTORIA MÚLTIPLE ==========
      real*8 points(3,max_points), vectors(3,max_points)
      real*8 segment_lengths(max_points)
      real*8 accumulated_length(max_points)
      real*8 total_length, laser_vector(3)
      real*8 segment_start_dist, local_dist
      integer num_points, current_segment
      logical first_call
      integer current_kstep  ! Para trackear el step actual
      real*8 step_start_time ! Tiempo de inicio del step actual
      integer ierr
      character*200 line
      character*200 clean

! ========== VARIABLES PRINCIPALES ==========
      real*8 Q0,RE,RI,ZE,ZI,X0,Y0,Z0,VY,XD,YD,ZD,SA,CA
      real*8 A1,A2,A3,XL,YL,ZL,DE,DI,R0,R02,XX,YY,ZZ,TT,R2
      real*8 effi,power,speed,weld_time,theta,alpha,start_time
      real*8 T_R, L_R, P_D, magnitude, weld_dist
      real*8 dist_x, dist_y, dist_z
      integer method, i, j, nvals
      logical is_last_point
      character*50 filename  ! Para diferentes archivos por step

! ========== INICIALIZACIÓN ==========
      save first_call, points, vectors, segment_lengths
      save accumulated_length, total_length, num_points
      save current_segment, current_kstep, step_start_time

      data first_call /.true./
      data num_points /0/
      data current_segment /1/
      data current_kstep /0/
      data step_start_time /0.0d0/

! ========== DETECTAR NUEVO STEP Y REINICIAR ==========
      if (kstep .ne. current_kstep) then
          first_call = .true.
          current_kstep = kstep
          step_start_time = time(2)  ! Guardar tiempo de inicio del step
          write(*,*) '=== INICIANDO NUEVO STEP:', kstep, '==='
          write(*,*) 'Tiempo de inicio del step:', step_start_time
      endif

! ========== LECTURA DE TRAYECTORIA MÚLTIPLE ==========
      if (first_call) then
          first_call = .false.

          ! Determinar nombre del archivo según el step
          ! Recomendado: I0 para steps > 9 (trayectoria_10.txt, etc.)
          write(filename, '("trayectoria_", I0, ".txt")') kstep

          write(*,*) 'Leyendo archivo:', trim(filename)

          open(unit=100, file=filename,
     &         status='old', action='read', iostat=ierr)
          if (ierr .ne. 0) then
              write(*,*) 'ERROR: No se puede abrir ', trim(filename)
              write(*,*) 'Creando archivo por defecto...'

              ! Crear archivo por defecto si no existe
              open(unit=100, file=filename, status='new')
              write(100,'(A)') 'X Y Z /// VX VY VZ'
              write(100,'(A)') '0.0 0.0 3.0 /// 0.0 0.0 -1.0'
              write(100,'(A)') '50.0 0.0 3.0 /// 0.0 0.0 -1.0'
              write(100,'(A)') '50.0 50.0 3.0 /// 0.0 0.0 -1.0'
              write(100,'(A)') '0.0 50.0 3.0'
              close(100)

              open(unit=100, file=filename, status='old', action='read')
          endif

          ! Leer puntos y vectores (acepta encabezado y separadores)
          num_points = 0
          do i = 1, max_points

              read(100, '(A)', iostat=ierr) line
              if (ierr .ne. 0) exit

              if (len_trim(line) .eq. 0) cycle

              ! Encontrar primer caracter no-espacio
              j = 1
              do while (j .le. len(line) .and. line(j:j) .eq. ' ')
                  j = j + 1
              enddo
              if (j .gt. len(line)) cycle

              ! Saltar comentarios y encabezados (líneas que no empiezan con número)
              if (line(j:j) .eq. '!' .or. line(j:j) .eq. '#') cycle
              if (.not. ((line(j:j) .ge. '0' .and. line(j:j) .le. '9')
     &            .or. line(j:j) .eq. '-' .or. line(j:j) .eq. '+'
     &            .or. line(j:j) .eq. '.')) then
                  cycle
              endif

              ! Sanitizar separadores: convierte / | , ; y tab en espacios
              clean = line
              do j = 1, len(clean)
                  if (clean(j:j) .eq. '/' .or. clean(j:j) .eq. '|'
     &                .or. clean(j:j) .eq. ',' .or. clean(j:j) .eq. ';'
     &                .or. clean(j:j) .eq. char(9)) then
                      clean(j:j) = ' '
                  endif
              enddo

              is_last_point = .false.

              ! Intentar leer 6 valores: XYZ + VXYZ
              read(clean, *, iostat=ierr)
     &             (points(j,num_points+1), j=1,3),
     &             (vectors(j,num_points+1), j=1,3)

              if (ierr .ne. 0) then
                  ! Si no, intentar leer solo XYZ (último punto)
                  read(clean, *, iostat=ierr)
     &                 (points(j,num_points+1), j=1,3)
                  if (ierr .ne. 0) then
                      write(*,*) 'ERROR: Línea con formato inválido:'
                      write(*,*) trim(line)
                      call exit(201)
                  endif
                  is_last_point = .true.

                  ! Vector del último punto = vector del punto anterior (o default)
                  if (num_points .ge. 1) then
                      vectors(1,num_points+1) = vectors(1,num_points)
                      vectors(2,num_points+1) = vectors(2,num_points)
                      vectors(3,num_points+1) = vectors(3,num_points)
                  else
                      vectors(1,num_points+1) = 0.0d0
                      vectors(2,num_points+1) = 0.0d0
                      vectors(3,num_points+1) = -1.0d0
                  endif
              endif

              num_points = num_points + 1

              ! Normalizar vector
              magnitude = vectors(1,num_points)**2
     &                 + vectors(2,num_points)**2
     &                 + vectors(3,num_points)**2
              magnitude = sqrt(magnitude)

              if (magnitude .gt. tolerance) then
		  vectors(1,num_points)=vectors(1,num_points)
     &                 /magnitude
                  vectors(2,num_points)=vectors(2,num_points)
     &                 /magnitude
                  vectors(3,num_points)=vectors(3,num_points)
     &                 /magnitude

              else
                  vectors(1,num_points) = 0.0d0
                  vectors(2,num_points) = 0.0d0
                  vectors(3,num_points) = -1.0d0
              endif

              if (is_last_point) exit
          enddo

          close(100)

          if (num_points .lt. 2) then
              write(*,*) 'ERROR: Se necesitan al menos 2 puntos'
              call exit(201)
          endif

          ! Calcular longitudes de segmentos
          total_length = 0.0d0
          do i = 1, num_points-1
              dist_x = points(1,i+1) - points(1,i)
              dist_y = points(2,i+1) - points(2,i)
              dist_z = points(3,i+1) - points(3,i)

              segment_lengths(i) = dist_x**2 + dist_y**2 + dist_z**2
              segment_lengths(i) = sqrt(segment_lengths(i))
              total_length = total_length + segment_lengths(i)

              if (i .eq. 1) then
                  accumulated_length(i) = segment_lengths(i)
              else
                  accumulated_length(i) = accumulated_length(i-1) +
     &                                   segment_lengths(i)
              endif
          enddo

          write(*,*) '=== TRAYECTORIA CARGADA PARA STEP', kstep, '==='
          write(*,*) 'Número de puntos:', num_points
          write(*,*) 'Longitud total:', total_length
      endif

! ========== PARÁMETROS LÁSER ==========
      speed = 70.0d0
      method = 1
      power = 3500.0d0
      effi = 0.225d0

      T_R = 0.857d0
      L_R = 0.738d0
      P_D = -2.25d0

! ========== INICIALIZACIÓN DE TIEMPO ==========
      start_time = 0.025d0
      ! Usar tiempo relativo al step actual
      weld_time = time(2) - step_start_time - start_time
      if(weld_time .lt. 0.0d0) return

! ========== DISTANCIA SOLDADA ==========
      weld_dist = speed * weld_time

! ========== VERIFICAR COMPLETACIÓN ==========
      if(weld_dist .gt. total_length) return

! ========== DETERMINAR SEGMENTO ACTUAL ==========
      current_segment = 1
      do i = 1, num_points-1
          if (weld_dist .le. accumulated_length(i)) then
              current_segment = i
              exit
          endif
      enddo

! ========== POSICIÓN ACTUAL DEL LÁSER ==========
      if (current_segment .eq. 1) then
          if (weld_dist .le. 0.0d0) then
              X0 = points(1,1)
              Y0 = points(2,1)
              Z0 = points(3,1)
          else
              dist_x = points(1,2) - points(1,1)
              dist_y = points(2,2) - points(2,1)
              dist_z = points(3,2) - points(3,1)

              X0 = points(1,1) + dist_x *
     &             (weld_dist / segment_lengths(1))
              Y0 = points(2,1) + dist_y *
     &             (weld_dist / segment_lengths(1))
              Z0 = points(3,1) + dist_z *
     &             (weld_dist / segment_lengths(1))
          endif
      else
          segment_start_dist = accumulated_length(current_segment-1)
          local_dist = weld_dist - segment_start_dist

          dist_x = points(1,current_segment+1) -
     &             points(1,current_segment)
          dist_y = points(2,current_segment+1) -
     &             points(2,current_segment)
          dist_z = points(3,current_segment+1) -
     &             points(3,current_segment)

          X0 = points(1,current_segment) + dist_x *
     &         (local_dist / segment_lengths(current_segment))
          Y0 = points(2,current_segment) + dist_y *
     &         (local_dist / segment_lengths(current_segment))
          Z0 = points(3,current_segment) + dist_z *
     &         (local_dist / segment_lengths(current_segment))
      endif

! ========== OBTENER VECTOR DEL LÁSER ==========
      laser_vector(1) = vectors(1,current_segment)
      laser_vector(2) = vectors(2,current_segment)
      laser_vector(3) = vectors(3,current_segment)

! ========== CALCULAR ÁNGULOS ==========
      dist_x = points(1,current_segment+1) -
     &         points(1,current_segment)
      dist_y = points(2,current_segment+1) -
     &         points(2,current_segment)

      if (abs(dist_x) .lt. tolerance) then
          theta = 1.570796327d0
      else
          theta = atan(dist_y / dist_x)
      endif

      alpha = atan2(laser_vector(2), laser_vector(1))

! ========== TRANSFORMACIÓN DE COORDENADAS ==========
      XX = coords(1) - X0
      YY = coords(2) - Y0
      ZZ = coords(3) - Z0

      XL = XX * cos(theta) + YY * sin(theta)
      YL = -XX * sin(theta) + YY * cos(theta)
      ZL = ZZ

! ========== SOLDADURA LÁSER ==========
      if (method .eq. 1) then
          Q0 = power * effi * 1000.0d0
          RE = T_R
          RI = L_R
          ZE = 0.0d0
          ZI = P_D
          VY = speed

          TT = weld_time - (YL / speed)

          if (TT .lt. 0.0d0) return

          SA = sin(alpha)
          CA = cos(alpha)

          XD = XL * CA + ZL * SA
          YD = YL
          ZD = -XL * SA + ZL * CA

          DE = ZD - ZE
          DI = ZD - ZI
          if (DE .gt. 0.00001d0) return
          if (DI + RI .lt. 0.00001d0) return

          A1 = XD * XD
          A2 = YD * YD
          R2 = A1 + A2

          if (ZD .le. ZI) then
              A3 = DI * DI
              R2 = R2 + 2.0d0 * A3
          endif

          A1 = RE - RI
          A2 = ZE - ZI
          A3 = ZE - ZD
          R0 = A3 / A2
          R0 = R0 * A1
          R0 = RE - R0
          if (ZD .le. ZI) R0 = RI
          R02 = R0 * R0

          if (R2 .gt. R02) return
          A1 = R2 / R02
          A2 = -1.0d0 * A1
          A2 = exp(A2)

          flux(1) = Q0 * A2
          return
      endif

      end
