  ÁX  Ä   k820309    é          2021.10.0   vÈf                                                                                                          
       time_step_alg_mod.f90 TIME_STEP_ALG_MOD              TIME_STEP #         @                                                               #TIME_STEP%DECOMPOSITION_TYPE    #TIME_STEP%SUBDOMAIN_TYPE    #TIME_STEP%C_PTR    #TIME_STEP%REGION_TYPE    #TIME_STEP%TILE_TYPE    #TIME_STEP%R2D_FIELD    #TIME_STEP%GRID_TYPE     #GRID À   #CURRENT Á   #TIME_STEPS Â                                            @   @              D                        'È              	      #GLOBAL_NX    #GLOBAL_NY    #NX    #NY    #NDOMAINS    #MAX_WIDTH    #MAX_HEIGHT 	   #SUBDOMAINS 
   #PROC_SUBDOMAINS                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    	                                                                     
                    0             #TIME_STEP%SUBDOMAIN_TYPE              &                                                         @   @                                      '0                    #GLOBAL    #INTERNAL                                                                                    #TIME_STEP%REGION_TYPE                   @   @                                      '                    #NX    #NY    #XSTART    #XSTOP    #YSTART    #YSTOP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              #TIME_STEP%REGION_TYPE                                                                   h              	               &                   &                                                         À   @                                     '                    #PTR                  D                                                                       @   @                                      '0                    #INTERNAL    #WHOLE                                                                                    #TIME_STEP%REGION_TYPE                                                                                   #TIME_STEP%REGION_TYPE                       @               À                       'p                   #FIELD_TYPE    #NTILES    #TILE    #DATA    #DEVICE_PTR    #SET_DATA    #GET_DATA    #READ_HALO_FROM_DEVICE    #WRITE_HALO_TO_DEVICE    #READ_FROM_DEVICE ¥   #WRITE_TO_DEVICE ­   #HALO_EXCHANGE µ   #GATHER_INNER_DATA »                 $                                            ¸                      #TIME_STEP%FIELD_TYPE                       @               D                        '¸                    #DEFINED_ON    #GRID    #INTERNAL c   #WHOLE d   #NUM_HALOS e   #HALO f   #DATA_ON_DEVICE k   #READ_FROM_DEVICE_C l   #READ_FROM_DEVICE_F t   #WRITE_TO_DEVICE_C |   #WRITE_TO_DEVICE_F                  $                                                                       $                                                                #TIME_STEP%GRID_TYPE                        @               À                         '             /      #NAME !   #OFFSET "   #GLOBAL_NX #   #GLOBAL_NY $   #NX %   #NY &   #DX '   #DY (   #TMASK )   #TMASK_DEVICE *   #BOUNDARY_CONDITIONS +   #SUBDOMAIN ,   #DECOMP -   #DX_T .   #DY_T /   #DX_T_DEVICE 0   #DY_T_DEVICE 1   #DX_U 2   #DY_U 3   #DX_U_DEVICE 4   #DY_U_DEVICE 5   #DX_V 6   #DY_V 7   #DX_V_DEVICE 8   #DY_V_DEVICE 9   #DX_F :   #DY_F ;   #DX_F_DEVICE <   #DY_F_DEVICE =   #AREA_T >   #AREA_U ?   #AREA_V @   #AREA_T_DEVICE A   #AREA_U_DEVICE B   #AREA_V_DEVICE C   #GPHIU D   #GPHIV E   #GPHIF F   #GPHIU_DEVICE G   #GPHIV_DEVICE H   #GPHIF_DEVICE I   #XT J   #YT K   #XT_DEVICE L   #YT_DEVICE M   #GET_TMASK N   #DECOMPOSE Q                 $                                       !                                 $                                       "                                $                                       #                                $                                       $                                $                                       %                                $                                       &                                $                                      '               
                 $                                      (                
               $                                       )            (              	               &                   &                                                         $                                      *                   
       #TIME_STEP%C_PTR                  $                                       +                               p          p            p                                        $                                       ,     0                     #TIME_STEP%SUBDOMAIN_TYPE                  $                                       -     È       Ð              #TIME_STEP%DECOMPOSITION_TYPE                $                                      .                            
            &                   &                                                       $                                      /            ø                
            &                   &                                                         $                                      0            X             #TIME_STEP%C_PTR                  $                                      1            `             #TIME_STEP%C_PTR                $                                      2            h                
            &                   &                                                       $                                      3            È                
            &                   &                                                         $                                      4            (             #TIME_STEP%C_PTR                  $                                      5            0             #TIME_STEP%C_PTR                $                                      6            8                
            &                   &                                                       $                                      7                            
            &                   &                                                         $                                      8            ø             #TIME_STEP%C_PTR                  $                                      9                          #TIME_STEP%C_PTR                $                                      :                            
            &                   &                                                       $                                      ;            h                
            &                   &                                                         $                                      <            È             #TIME_STEP%C_PTR                  $                                      =            Ð             #TIME_STEP%C_PTR                $                                      >            Ø                
            &                   &                                                       $                                      ?            8                
            &                   &                                                       $                                      @                             
            &                   &                                                         $                                      A            ø      !       #TIME_STEP%C_PTR                  $                                      B                   "       #TIME_STEP%C_PTR                  $                                      C                  #       #TIME_STEP%C_PTR                $                                      D                         $   
            &                   &                                                       $                                      E            p             %   
            &                   &                                                       $                                      F            Ð             &   
            &                   &                                                         $                                      G            0      '       #TIME_STEP%C_PTR                  $                                      H            8      (       #TIME_STEP%C_PTR                  $                                      I            @      )       #TIME_STEP%C_PTR                $                                      J            H             *   
            &                   &                                                       $                                      K            ¨             +   
            &                   &                                                         $                                      L                  ,       #TIME_STEP%C_PTR                  $                                      M                  -       #TIME_STEP%C_PTR    1         À    $                                    N             .     #GET_TMASK O   (         D   @                                    O                                       #SELF P             &                   &                                                     
                                          P                  #GRID_TYPE     1         À    $                                     Q             /     #DECOMPOSE R   #         @     @                                     R                   #TIME_STEP%DECOMPOSE%MAP_COMMS S   #TIME_STEP%DECOMPOSE%PARALLEL_FINALISE Y   #TIME_STEP%DECOMPOSE%PARALLEL_ABORT Z   #SELF \   #DOMAINX ]   #DOMAINY ^   #NDOMAINS _   #NDOMAINX `   #NDOMAINY a   #HALO_WIDTH b   #        @                                           S                    #DECOMP T   #TMASK U   #PBC V   #HALO_DEPTHS W   #IERR X             
  `                                        T     È              #TIME_STEP%DECOMPOSITION_TYPE            
                                           U                    $             &                   &                                                     
                                           V                     
                                           W                    %   p          p            p                                     @                                        X            #        @                                           Y                     #        @                                           Z                    #MSG [             
                                         [                    1           
                                         \                   #GRID_TYPE               
  @                                        ]                     
  @                                        ^                     
 @                                        _                     
 @                                        `                     
 @                                        a                     
 @                                        b                         $                                       c                          #TIME_STEP%REGION_TYPE                  $                                       d            (              #TIME_STEP%REGION_TYPE                  $                                       e     @                         $                                       f            H       4             #TIME_STEP%HALO_TYPE g             &                                                          @   @                                 g     '4                    #NEEDS_UPDATE h   #SOURCE i   #DEST j                 $                                       h                                 $                                       i                          #TIME_STEP%REGION_TYPE                  $                                       j                          #TIME_STEP%REGION_TYPE                  $                                       k                  #         @    $                      d               l                    #FROM m   #TO n   #STARTX o   #STARTY p   #NX q   #NY r   #BLOCKING s                     
                                       m                    #TIME_STEP%C_PTR              
                                       n                    #TIME_STEP%C_PTR              
                                       o                      
                                       p                      
                                       q                      
                                       r                      
                                       s             #         @    $                      d               t                    #FROM u   #TO v   #STARTX w   #STARTY x   #NX y   #NY z   #BLOCKING {   	                   
                                        u                   #TIME_STEP%C_PTR              
                                       v                   
               &                   &                                                     
                                         w                     
                                         x                     
                                         y                     
                                         z                     
                                         {           #         @    $                      d               |                    #FROM }   #TO ~   #STARTX    #STARTY    #NX    #NY    #BLOCKING    
 ¨                 
                                       }                    #TIME_STEP%C_PTR              
                                       ~                    #TIME_STEP%C_PTR              
                                                             
                                                             
                                                             
                                                             
                                                    #         @    $                      d                                   #FROM    #TO    #STARTX    #STARTY    #NX    #NY    #BLOCKING     °                 
                                                           
              &                   &                                                     
                                                           #TIME_STEP%C_PTR              
                                                              
                                                              
                                                              
                                                              
                                                                  $                                            ¸                         $                                                   À       0             #TIME_STEP%TILE_TYPE              &                                                      $                                                                  
            &                   &                                                         $                                                  h             #TIME_STEP%C_PTR    1         À    $                                                      #SET_DATA    %         @    @                                                               #SELF    #ARRAY               @                                            p              #R2D_FIELD                                                                          
 
              &                   &                                           1         À    $                                                      #GET_DATA    (         D   @                                                       	                
    #SELF              &                   &                                                      `                                            p              #R2D_FIELD    1         À    $                                                       #READ_HALO_FROM_DEVICE    #         @     @                                                        #TIME_STEP%READ_HALO_FROM_DEVICE%GLOBAL_SUM    #SELF    #COMM    #BLOCKING    #        @                                                               #VAR              
                                              
                 
 `                                            p              #R2D_FIELD              
                                                                
  @                                                   1         À    $                                                  	     #WRITE_HALO_TO_DEVICE    #         @     @                                                        #TIME_STEP%WRITE_HALO_TO_DEVICE%GLOBAL_SUM     #SELF ¢   #COMM £   #BLOCKING ¤   #        @                                                                #VAR ¡             
                                         ¡     
                 
 `                                       ¢     p              #R2D_FIELD              
                                           £                     
  @                                        ¤           1         À    $                                     ¥             
     #READ_FROM_DEVICE ¦   #         @     @                                     ¦                    #SELF §   #STARTX ¨   #STARTY ©   #NX ª   #NY «   #BLOCKING ¬              `                                       §     p              #R2D_FIELD              
 @                                        ¨                     
 @                                        ©                     
 @                                        ª                     
 @                                        «                     
 @                                        ¬           1         À    $                                     ­                  #WRITE_TO_DEVICE ®   #         @     @                                     ®                    #SELF ¯   #STARTX °   #STARTY ±   #NX ²   #NY ³   #BLOCKING ´              `                                       ¯     p              #R2D_FIELD              
 @                                        °                     
 @                                        ±                     
 @                                        ²                     
 @                                        ³                     
 @                                        ´           1         À    $                                     µ                  #HALO_EXCHANGE ¶   #         @     @                                     ¶                   #TIME_STEP%HALO_EXCHANGE%GLOBAL_SUM ·   #SELF ¹   #DEPTH º   #        @                                           ·                    #VAR ¸             
                                         ¸     
                 
 `                                       ¹     p              #R2D_FIELD              
                                           º           1         À    $                                     »                  #GATHER_INNER_DATA ¼   #         @     @                                     ¼                   #TIME_STEP%GATHER_INNER_DATA%GET_NUM_RANKS ½   #SELF ¾   #GLOBAL_DATA ¿   %        @                                         ½                                      
                                          ¾     p             #R2D_FIELD                                                     ¿                   
               &                   &                                                     
  `                                        À                  #TIME_STEP%GRID_TYPE               
D @                                        Á     p              #TIME_STEP%R2D_FIELD              
                                           Â                  0      fn#fn '   Ð      b   uapp(TIME_STEP_ALG_MOD    ê   M      TIME_STEP R   7  Ø      TIME_STEP%DECOMPOSITION_TYPE+DECOMPOSITION_MOD=DECOMPOSITION_TYPE I     P   a   TIME_STEP%DECOMPOSITION_TYPE%GLOBAL_NX+DECOMPOSITION_MOD I   _  P   a   TIME_STEP%DECOMPOSITION_TYPE%GLOBAL_NY+DECOMPOSITION_MOD B   ¯  P   a   TIME_STEP%DECOMPOSITION_TYPE%NX+DECOMPOSITION_MOD B   ÿ  P   a   TIME_STEP%DECOMPOSITION_TYPE%NY+DECOMPOSITION_MOD H   O  P   a   TIME_STEP%DECOMPOSITION_TYPE%NDOMAINS+DECOMPOSITION_MOD I     P   a   TIME_STEP%DECOMPOSITION_TYPE%MAX_WIDTH+DECOMPOSITION_MOD J   ï  P   a   TIME_STEP%DECOMPOSITION_TYPE%MAX_HEIGHT+DECOMPOSITION_MOD J   ?  º   a   TIME_STEP%DECOMPOSITION_TYPE%SUBDOMAINS+DECOMPOSITION_MOD J   ù  r      TIME_STEP%SUBDOMAIN_TYPE+DECOMPOSITION_MOD=SUBDOMAIN_TYPE B   k  s   a   TIME_STEP%SUBDOMAIN_TYPE%GLOBAL+DECOMPOSITION_MOD =   Þ        TIME_STEP%REGION_TYPE+REGION_MOD=REGION_TYPE 4   t  P   a   TIME_STEP%REGION_TYPE%NX+REGION_MOD 4   Ä  P   a   TIME_STEP%REGION_TYPE%NY+REGION_MOD 8     P   a   TIME_STEP%REGION_TYPE%XSTART+REGION_MOD 7   d  P   a   TIME_STEP%REGION_TYPE%XSTOP+REGION_MOD 8   ´  P   a   TIME_STEP%REGION_TYPE%YSTART+REGION_MOD 7   	  P   a   TIME_STEP%REGION_TYPE%YSTOP+REGION_MOD D   T	  s   a   TIME_STEP%SUBDOMAIN_TYPE%INTERNAL+DECOMPOSITION_MOD O   Ç	  ´   a   TIME_STEP%DECOMPOSITION_TYPE%PROC_SUBDOMAINS+DECOMPOSITION_MOD 4   {
  a      TIME_STEP%C_PTR+ISO_C_BINDING=C_PTR 6   Ü
  P   %   TIME_STEP%C_PTR%PTR+ISO_C_BINDING=PTR 7   ,  q      TIME_STEP%TILE_TYPE+TILE_MOD=TILE_TYPE 6     s   a   TIME_STEP%TILE_TYPE%INTERNAL+TILE_MOD 3     s   a   TIME_STEP%TILE_TYPE%WHOLE+TILE_MOD .     >     TIME_STEP%R2D_FIELD+FIELD_MOD 9   Á  r   a   TIME_STEP%R2D_FIELD%FIELD_TYPE+FIELD_MOD :   3       TIME_STEP%FIELD_TYPE+FIELD_MOD=FIELD_TYPE :   I  P   a   TIME_STEP%FIELD_TYPE%DEFINED_ON+FIELD_MOD 4     q   a   TIME_STEP%FIELD_TYPE%GRID+FIELD_MOD -   
  Õ     TIME_STEP%GRID_TYPE+GRID_MOD 2   ß  P   a   TIME_STEP%GRID_TYPE%NAME+GRID_MOD 4   /  P   a   TIME_STEP%GRID_TYPE%OFFSET+GRID_MOD 7     P   a   TIME_STEP%GRID_TYPE%GLOBAL_NX+GRID_MOD 7   Ï  P   a   TIME_STEP%GRID_TYPE%GLOBAL_NY+GRID_MOD 0     P   a   TIME_STEP%GRID_TYPE%NX+GRID_MOD 0   o  P   a   TIME_STEP%GRID_TYPE%NY+GRID_MOD 0   ¿  P   a   TIME_STEP%GRID_TYPE%DX+GRID_MOD 0     P   a   TIME_STEP%GRID_TYPE%DY+GRID_MOD 3   _  ´   a   TIME_STEP%GRID_TYPE%TMASK+GRID_MOD :     m   a   TIME_STEP%GRID_TYPE%TMASK_DEVICE+GRID_MOD A     ¤   a   TIME_STEP%GRID_TYPE%BOUNDARY_CONDITIONS+GRID_MOD 7   $  v   a   TIME_STEP%GRID_TYPE%SUBDOMAIN+GRID_MOD 4     z   a   TIME_STEP%GRID_TYPE%DECOMP+GRID_MOD 2     ´   a   TIME_STEP%GRID_TYPE%DX_T+GRID_MOD 2   È  ´   a   TIME_STEP%GRID_TYPE%DY_T+GRID_MOD 9   |  m   a   TIME_STEP%GRID_TYPE%DX_T_DEVICE+GRID_MOD 9   é  m   a   TIME_STEP%GRID_TYPE%DY_T_DEVICE+GRID_MOD 2   V  ´   a   TIME_STEP%GRID_TYPE%DX_U+GRID_MOD 2   
  ´   a   TIME_STEP%GRID_TYPE%DY_U+GRID_MOD 9   ¾  m   a   TIME_STEP%GRID_TYPE%DX_U_DEVICE+GRID_MOD 9   +  m   a   TIME_STEP%GRID_TYPE%DY_U_DEVICE+GRID_MOD 2     ´   a   TIME_STEP%GRID_TYPE%DX_V+GRID_MOD 2   L  ´   a   TIME_STEP%GRID_TYPE%DY_V+GRID_MOD 9      m   a   TIME_STEP%GRID_TYPE%DX_V_DEVICE+GRID_MOD 9   m  m   a   TIME_STEP%GRID_TYPE%DY_V_DEVICE+GRID_MOD 2   Ú  ´   a   TIME_STEP%GRID_TYPE%DX_F+GRID_MOD 2     ´   a   TIME_STEP%GRID_TYPE%DY_F+GRID_MOD 9   B   m   a   TIME_STEP%GRID_TYPE%DX_F_DEVICE+GRID_MOD 9   ¯   m   a   TIME_STEP%GRID_TYPE%DY_F_DEVICE+GRID_MOD 4   !  ´   a   TIME_STEP%GRID_TYPE%AREA_T+GRID_MOD 4   Ð!  ´   a   TIME_STEP%GRID_TYPE%AREA_U+GRID_MOD 4   "  ´   a   TIME_STEP%GRID_TYPE%AREA_V+GRID_MOD ;   8#  m   a   TIME_STEP%GRID_TYPE%AREA_T_DEVICE+GRID_MOD ;   ¥#  m   a   TIME_STEP%GRID_TYPE%AREA_U_DEVICE+GRID_MOD ;   $  m   a   TIME_STEP%GRID_TYPE%AREA_V_DEVICE+GRID_MOD 3   $  ´   a   TIME_STEP%GRID_TYPE%GPHIU+GRID_MOD 3   3%  ´   a   TIME_STEP%GRID_TYPE%GPHIV+GRID_MOD 3   ç%  ´   a   TIME_STEP%GRID_TYPE%GPHIF+GRID_MOD :   &  m   a   TIME_STEP%GRID_TYPE%GPHIU_DEVICE+GRID_MOD :   '  m   a   TIME_STEP%GRID_TYPE%GPHIV_DEVICE+GRID_MOD :   u'  m   a   TIME_STEP%GRID_TYPE%GPHIF_DEVICE+GRID_MOD 0   â'  ´   a   TIME_STEP%GRID_TYPE%XT+GRID_MOD 0   (  ´   a   TIME_STEP%GRID_TYPE%YT+GRID_MOD 7   J)  m   a   TIME_STEP%GRID_TYPE%XT_DEVICE+GRID_MOD 7   ·)  m   a   TIME_STEP%GRID_TYPE%YT_DEVICE+GRID_MOD 7   $*  _   a   TIME_STEP%GRID_TYPE%GET_TMASK+GRID_MOD 7   *  Æ      TIME_STEP%GET_TMASK+GRID_MOD=GET_TMASK 2   I+  _   a   TIME_STEP%GET_TMASK%SELF+GRID_MOD 7   ¨+  _   a   TIME_STEP%GRID_TYPE%DECOMPOSE+GRID_MOD 7   ,  $     TIME_STEP%DECOMPOSE+GRID_MOD=DECOMPOSE K   +-        TIME_STEP%DECOMPOSE%MAP_COMMS+PARALLEL_COMMS_MOD=MAP_COMMS H   ¶-  r   a   TIME_STEP%DECOMPOSE%MAP_COMMS%DECOMP+PARALLEL_COMMS_MOD G   (.  ¬   a   TIME_STEP%DECOMPOSE%MAP_COMMS%TMASK+PARALLEL_COMMS_MOD E   Ô.  H   a   TIME_STEP%DECOMPOSE%MAP_COMMS%PBC+PARALLEL_COMMS_MOD M   /     a   TIME_STEP%DECOMPOSE%MAP_COMMS%HALO_DEPTHS+PARALLEL_COMMS_MOD F   ¸/  H   a   TIME_STEP%DECOMPOSE%MAP_COMMS%IERR+PARALLEL_COMMS_MOD [    0  P      TIME_STEP%DECOMPOSE%PARALLEL_FINALISE+PARALLEL_UTILS_MOD=PARALLEL_FINALISE U   P0  Y      TIME_STEP%DECOMPOSE%PARALLEL_ABORT+PARALLEL_UTILS_MOD=PARALLEL_ABORT J   ©0  T   a   TIME_STEP%DECOMPOSE%PARALLEL_ABORT%MSG+PARALLEL_UTILS_MOD 2   ý0  _   a   TIME_STEP%DECOMPOSE%SELF+GRID_MOD 5   \1  H   a   TIME_STEP%DECOMPOSE%DOMAINX+GRID_MOD 5   ¤1  H   a   TIME_STEP%DECOMPOSE%DOMAINY+GRID_MOD 6   ì1  H   a   TIME_STEP%DECOMPOSE%NDOMAINS+GRID_MOD 6   42  H   a   TIME_STEP%DECOMPOSE%NDOMAINX+GRID_MOD 6   |2  H   a   TIME_STEP%DECOMPOSE%NDOMAINY+GRID_MOD 8   Ä2  H   a   TIME_STEP%DECOMPOSE%HALO_WIDTH+GRID_MOD 8   3  s   a   TIME_STEP%FIELD_TYPE%INTERNAL+FIELD_MOD 5   3  s   a   TIME_STEP%FIELD_TYPE%WHOLE+FIELD_MOD 9   ò3  P   a   TIME_STEP%FIELD_TYPE%NUM_HALOS+FIELD_MOD 4   B4  µ   a   TIME_STEP%FIELD_TYPE%HALO+FIELD_MOD 7   ÷4        TIME_STEP%HALO_TYPE+HALO_MOD=HALO_TYPE :   w5  P   a   TIME_STEP%HALO_TYPE%NEEDS_UPDATE+HALO_MOD 4   Ç5  s   a   TIME_STEP%HALO_TYPE%SOURCE+HALO_MOD 2   :6  s   a   TIME_STEP%HALO_TYPE%DEST+HALO_MOD >   ­6  P   a   TIME_STEP%FIELD_TYPE%DATA_ON_DEVICE+FIELD_MOD U   ý6  ¢      TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C+FIELD_MOD=READ_FROM_DEVICE_C G   7  e   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%FROM+FIELD_MOD E   8  e   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%TO+FIELD_MOD I   i8  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%STARTX+FIELD_MOD I   ±8  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%STARTY+FIELD_MOD E   ù8  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%NX+FIELD_MOD E   A9  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%NY+FIELD_MOD K   9  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_C%BLOCKING+FIELD_MOD U   Ñ9  ¢      TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F+FIELD_MOD=READ_FROM_DEVICE_F G   s:  e   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%FROM+FIELD_MOD E   Ø:  ¬   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%TO+FIELD_MOD I   ;  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%STARTX+FIELD_MOD I   Ì;  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%STARTY+FIELD_MOD E   <  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%NX+FIELD_MOD E   \<  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%NY+FIELD_MOD K   ¤<  H   a   TIME_STEP%FIELD_TYPE%READ_FROM_DEVICE_F%BLOCKING+FIELD_MOD S   ì<  ¢      TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C+FIELD_MOD=WRITE_TO_DEVICE_C F   =  e   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%FROM+FIELD_MOD D   ó=  e   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%TO+FIELD_MOD H   X>  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%STARTX+FIELD_MOD H    >  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%STARTY+FIELD_MOD D   è>  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%NX+FIELD_MOD D   0?  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%NY+FIELD_MOD J   x?  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_C%BLOCKING+FIELD_MOD S   À?  ¢      TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F+FIELD_MOD=WRITE_TO_DEVICE_F F   b@  ¬   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%FROM+FIELD_MOD D   A  e   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%TO+FIELD_MOD H   sA  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%STARTX+FIELD_MOD H   »A  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%STARTY+FIELD_MOD D   B  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%NX+FIELD_MOD D   KB  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%NY+FIELD_MOD J   B  H   a   TIME_STEP%FIELD_TYPE%WRITE_TO_DEVICE_F%BLOCKING+FIELD_MOD 5   ÛB  P   a   TIME_STEP%R2D_FIELD%NTILES+FIELD_MOD 3   +C  µ   a   TIME_STEP%R2D_FIELD%TILE+FIELD_MOD 3   àC  ´   a   TIME_STEP%R2D_FIELD%DATA+FIELD_MOD 9   D  m   a   TIME_STEP%R2D_FIELD%DEVICE_PTR+FIELD_MOD 7   E  ^   a   TIME_STEP%R2D_FIELD%SET_DATA+FIELD_MOD 6   _E  m      TIME_STEP%SET_DATA+FIELD_MOD=SET_DATA 2   ÌE  _   a   TIME_STEP%SET_DATA%SELF+FIELD_MOD 3   +F  ¬   a   TIME_STEP%SET_DATA%ARRAY+FIELD_MOD 7   ×F  ^   a   TIME_STEP%R2D_FIELD%GET_DATA+FIELD_MOD 6   5G  Æ      TIME_STEP%GET_DATA+FIELD_MOD=GET_DATA 2   ûG  _   a   TIME_STEP%GET_DATA%SELF+FIELD_MOD D   ZH  k   a   TIME_STEP%R2D_FIELD%READ_HALO_FROM_DEVICE+FIELD_MOD P   ÅH  ¢      TIME_STEP%READ_HALO_FROM_DEVICE+FIELD_MOD=READ_HALO_FROM_DEVICE Y   gI  Y      TIME_STEP%READ_HALO_FROM_DEVICE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM R   ÀI  H   a   TIME_STEP%READ_HALO_FROM_DEVICE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD ?   J  _   a   TIME_STEP%READ_HALO_FROM_DEVICE%SELF+FIELD_MOD ?   gJ  H   a   TIME_STEP%READ_HALO_FROM_DEVICE%COMM+FIELD_MOD C   ¯J  H   a   TIME_STEP%READ_HALO_FROM_DEVICE%BLOCKING+FIELD_MOD C   ÷J  j   a   TIME_STEP%R2D_FIELD%WRITE_HALO_TO_DEVICE+FIELD_MOD N   aK  ¡      TIME_STEP%WRITE_HALO_TO_DEVICE+FIELD_MOD=WRITE_HALO_TO_DEVICE X   L  Y      TIME_STEP%WRITE_HALO_TO_DEVICE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM Q   [L  H   a   TIME_STEP%WRITE_HALO_TO_DEVICE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD >   £L  _   a   TIME_STEP%WRITE_HALO_TO_DEVICE%SELF+FIELD_MOD >   M  H   a   TIME_STEP%WRITE_HALO_TO_DEVICE%COMM+FIELD_MOD B   JM  H   a   TIME_STEP%WRITE_HALO_TO_DEVICE%BLOCKING+FIELD_MOD ?   M  f   a   TIME_STEP%R2D_FIELD%READ_FROM_DEVICE+FIELD_MOD F   øM        TIME_STEP%READ_FROM_DEVICE+FIELD_MOD=READ_FROM_DEVICE :   N  _   a   TIME_STEP%READ_FROM_DEVICE%SELF+FIELD_MOD <   çN  H   a   TIME_STEP%READ_FROM_DEVICE%STARTX+FIELD_MOD <   /O  H   a   TIME_STEP%READ_FROM_DEVICE%STARTY+FIELD_MOD 8   wO  H   a   TIME_STEP%READ_FROM_DEVICE%NX+FIELD_MOD 8   ¿O  H   a   TIME_STEP%READ_FROM_DEVICE%NY+FIELD_MOD >   P  H   a   TIME_STEP%READ_FROM_DEVICE%BLOCKING+FIELD_MOD >   OP  e   a   TIME_STEP%R2D_FIELD%WRITE_TO_DEVICE+FIELD_MOD D   ´P        TIME_STEP%WRITE_TO_DEVICE+FIELD_MOD=WRITE_TO_DEVICE 9   DQ  _   a   TIME_STEP%WRITE_TO_DEVICE%SELF+FIELD_MOD ;   £Q  H   a   TIME_STEP%WRITE_TO_DEVICE%STARTX+FIELD_MOD ;   ëQ  H   a   TIME_STEP%WRITE_TO_DEVICE%STARTY+FIELD_MOD 7   3R  H   a   TIME_STEP%WRITE_TO_DEVICE%NX+FIELD_MOD 7   {R  H   a   TIME_STEP%WRITE_TO_DEVICE%NY+FIELD_MOD =   ÃR  H   a   TIME_STEP%WRITE_TO_DEVICE%BLOCKING+FIELD_MOD <   S  c   a   TIME_STEP%R2D_FIELD%HALO_EXCHANGE+FIELD_MOD @   nS        TIME_STEP%HALO_EXCHANGE+FIELD_MOD=HALO_EXCHANGE Q   ûS  Y      TIME_STEP%HALO_EXCHANGE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM J   TT  H   a   TIME_STEP%HALO_EXCHANGE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD 7   T  _   a   TIME_STEP%HALO_EXCHANGE%SELF+FIELD_MOD 8   ûT  H   a   TIME_STEP%HALO_EXCHANGE%DEPTH+FIELD_MOD @   CU  g   a   TIME_STEP%R2D_FIELD%GATHER_INNER_DATA+FIELD_MOD H   ªU        TIME_STEP%GATHER_INNER_DATA+FIELD_MOD=GATHER_INNER_DATA [   DV  X      TIME_STEP%GATHER_INNER_DATA%GET_NUM_RANKS+PARALLEL_UTILS_MOD=GET_NUM_RANKS ;   V  _   a   TIME_STEP%GATHER_INNER_DATA%SELF+FIELD_MOD B   ûV  ¬   a   TIME_STEP%GATHER_INNER_DATA%GLOBAL_DATA+FIELD_MOD    §W  i   a   TIME_STEP%GRID "   X  i   a   TIME_STEP%CURRENT %   yX  H   a   TIME_STEP%TIME_STEPS 