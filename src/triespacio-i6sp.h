! triespacio.h
!------------------------------------------------------------------------------
! Espacio tridimensional
! Esta librería precisa de la librería array.
!
! El objetivo es aparentar que una localidad (una nave, un barco, un coche ...)
! se mueve por un espacio tridimensional.
!
! Se definen las clases espacio y ente. En un mismo espacio, pueden moverse más
! de un ente.
!
! Los obstáculos se definen como arrays de nx4, de forma que hay una fila por
! cada obstáculo. El valor 1 define la posicion x del obstáculo, el valor 2, el x2,
! la y, el valor 3, el valor 4 el y2, la z en el valor 5, y el valor 6 la z2.
! No se permitirá al jugador moverse allí.
! En general, para utilizar esta librería sólo es necesario instanciar un objeto
! mapa y otro objeto ente. Véase demoEspacio.inf para más detalles.
!
! (c) Baltasar, el arquero. baltasarq@yahoo.es 2002
!------------------------------------------------------------------------------

System_file;

IFNDEF TRIESPACIO_H;
Constant TRIESPACIO_H;

Message "Compilando módulo de espacios tridimensionales. v020811";

Include "Array";

! -----------------------------------------------------------------------------
! Clase para el espacio.
! -----------------------------------------------------------------------------

class espacio
private
    maxx 0,
    maxy 0,
    maxz 0,
    obstaculos 0,
    zonas 0,
    fueObst -1,
    fueZona -1,
      ult_movil 0,
with
      ! ---------------------- Límites ---------------------------------
    devMaxx [; return self.maxx; ],
    devMaxy [; return self.maxy; ],
    devMaxz [; return self.maxz; ],

      ! ---------------------- Acciones cuando se cumple algo ----------
      siXesMaxX [; rtrue; ],
      siYesMaxY [; rtrue; ],
      siZesMaxZ [; rtrue; ],

      siXesMinX [; rtrue; ],
      siYesMinY [; rtrue; ],
      siZesMinZ [; rtrue; ],

      siZonaEnc [; rtrue; ],
      siObstEnc [; rtrue; ],

    dev_fueZona [; return self.fueZona; ],
    dev_fueObst [; return self.fueObst; ],

      ! ---------------------- Movimiento ------------------------------
    dev_ultMovil [; return self.ult_movil; ],
    mueve [movil; 
                  self.ult_movil = movil;

            if (~~movil.puede_moverse)
                return false;


            if (movil.z>=self.maxz)
            {
                movil.retrocede();
                        movil.siZesMaxZ();
                return self.siZesMaxz();
            }
            else
            if (movil.x>=self.maxx)
            {
                movil.retrocede();
                        movil.siXesMaxX();
                return self.siXesMaxx();
            }
            else
            if (movil.y>=self.maxy)
            {
                movil.retrocede();
                        movil.siYesMaxY();
                return self.siYesMaxY();    
            }
            else 
            if (movil.z<0)
            {
                movil.retrocede();
                        movil.siZesMinZ();
                return self.siZesMinZ();
            }
            else
            if(movil.y<0)
            {
                movil.retrocede();
                        movil.siYesMinY();
                return self.siYesMinY();
            }
            else
            if(movil.x<0)
            {
                movil.retrocede();
                        self.siXesMinX();
                return self.siXesMinX();
            }

            ! Debemos comprobar SIEMPRE los obstáculos
            if (self.compObst(movil) >= 0)
            {
                return false;
            }

            self.compZona(movil);

            rtrue; 
    ],
    compObst [ movil i;
      ! Devuelve el número de obstáculo encontrado -------------------------
            self.fueObst = -1;
        if (self.obstaculos ofclass coleccion)
        {
          ! print "Num obst: ", self.obstaculos.dev_dimx(), "^";
          for(i=0:i<self.obstaculos.dev_dimx() && ( self.fueObst == -1 ):i++)
          {
            !print "Comprobando ", movil.x, ", ", movil.y, " en ",
            !    self.obstaculos.elemento(i, 0), "-",
            !    self.obstaculos.elemento(i, 1), ", ",
            !    self.obstaculos.elemento(i, 2), "-",
            !    self.obstaculos.elemento(i, 3), ", ",
            !    self.obstaculos.elemento(i, 4), "-",
            !    self.obstaculos.elemento(i, 5), "^"
            !;


            if (movil.x >= self.obstaculos.elemento(i, 0)
                   && movil.x <= self.obstaculos.elemento(i, 1)
                   && movil.y >= self.obstaculos.elemento(i, 2)
                   && movil.y <= self.obstaculos.elemento(i, 3)
                   && movil.z >= self.obstaculos.elemento(i, 4)
                   && movil.z <= self.obstaculos.elemento(i, 5))
                   {
                self.siObstEnc();
                movil.retrocede();
                movil.siObstEnc();
                self.fueObst = i;
             }
          }
        }
        
        ! print "Obstaculo: ", self.fueObst, "^";
        return self.fueObst;
    ],
    compZona [ movil i;
            self.fueZona = -1;
        if (self.zonas ofclass coleccion)
        {
          for(i=0:i<self.zonas.dev_dimx() && self.fueZona==-1:i++)
          {

            if (movil.x >= self.zonas.elemento(i, 0)
                   && movil.x <= self.zonas.elemento(i, 1)
                   && movil.y >= self.zonas.elemento(i, 2)
                   && movil.y <= self.zonas.elemento(i, 3)
                   && movil.z >= self.zonas.elemento(i, 4)
                   && movil.z <= self.zonas.elemento(i, 5))
                   {
                self.fueZona = i;
                self.siZonaEnc();
                        movil.siZonaEnc();
             }
    

           }
        }

        return self.fueZona;
    ]
;

! -----------------------------------------------------------------------------
! Clase ente, la que se mueve por el mapa.
! -----------------------------------------------------------------------------

class ente
private
    mapa 0,
    ultmov 0,    ! Puede ser 0,1,2,3, para n,s,e,o. 4,5,6,7 para ne,no,se,so
            ! y 8,9 para arriba/abajo. Lo mismo el par. para posRel
            ! 10 significa que el último movimiento fue absoluto.
            ! 11 significa que se retrocedió.
    ultx 0,
    ulty 0,
    ultz 0
    
with
      ! -------------- Acciones a cumplir si se cumple alguna condición ----
    siZesMaxz [; return false; ],
    siXesMaxx [; return false; ],
    siYesMaxy [; return false; ],
    siZesMinz [; return false; ],
    siXesMinx [; return false; ],
    siYesMiny [; return false; ],
    siObstEnc [; return false; ],
    siZonaEnc [; return false; ],

      ! -------------- Movimiento del ente ---------------------------------
    dev_ultX [; return self.ultx; ],
    dev_ultY [; return self.ulty; ],
    dev_ultZ [; return self.ultz; ],
    dev_ultmov [; return self.ultmov; ],

    accel 1,                        ! Lo rápido que va el ente
    x 0,
    y 0,
    z 0,
    puede_moverse true,                ! puede hacer movimientos ?

    posRel [mov dist; 

        if (~~self.puede_moverse)
            return;
    
        self.ultmov = mov;                ! 0 a 9. 10 no !
        self.ultx = self.x;
        self.ulty = self.y;
        self.ultz = self.z;
        dist      = self.accel;

        if (self.ultmov == 0)
            self.y = self.y + dist;
        else
        if (self.ultmov == 1)
            self.y = self.y - dist;
        else
        if (self.ultmov == 2)
            self.x = self.x + dist;
        else
        if (self.ultmov == 3)
            self.x = self.x - dist;
        else
        if (self.ultmov == 4)
        {
            self.y = self.y + dist;
            self.x = self.x + dist;
        }
        else
        if (self.ultmov == 5)
        {
            self.y = self.y + dist;
            self.x = self.x - dist;
        }
        else
        if (self.ultmov == 6)
        {
            self.y = self.y - dist;
            self.x = self.x + dist;
        }
        else
        if (self.ultmov == 7)
        {
            self.y = self.y - dist;
            self.x = self.x - dist;
        }
        if (self.ultmov == 8)
            self.z = self.z + dist;
        else
        if (self.ultmov == 9)
            self.z = self.z - dist;
    ],
    posAbs [x y z; 

        if (~~self.puede_moverse)
            return;

        self.ultmov = 10;            ! Automáticamente, es 10
        self.ultx = self.x;
        self.ulty = self.y;
        self.ultz = self.z;

        self.x    = x;
        self.y    = y;
        self.z    = z;
    ],
    retrocede [;
        self.x = self.ultx;
        self.y = self.ulty;
        self.z = self.ultz;
        self.ultmov = 11;
    ],

      movimientoRel [ dir;
            self.posRel(dir); return self.siMueve(self.mapa.mueve(self));
      ],

      movimientoAbs [ x y z;
            self.posAbs(x, y, z); return self.siMueve(self.mapa.mueve(self));
      ],

    s_to  [; return self.movimientoRel(1);],
    n_to  [; return self.movimientoRel(0);],
    e_to  [; return self.movimientoRel(2);],
    w_to  [; return self.movimientoRel(3);],

    nw_to [; return self.movimientoRel(5);],
    ne_to [; return self.movimientoRel(4);],
    se_to [; return self.movimientoRel(6);],
    sw_to [; return self.movimientoRel(7);],

    u_to [; return self.movimientoRel(8);],
    d_to  [; return self.movimientoRel(9);],

    siMueve [ moved; return moved; ]
;

ENDIF;
