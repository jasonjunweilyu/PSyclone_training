  ·i  í   k820309    é          2021.10.0   vÈf                                                                                                          
       time_step_alg_mod_psy.f90 PSY_TIME_STEP_ALG_MOD                      @                                      
                                                                   
                          @               À                       'p                   #FIELD_TYPE    #NTILES    #TILE    #DATA    #DEVICE_PTR    #SET_DATA    #GET_DATA    #READ_HALO_FROM_DEVICE    #WRITE_HALO_TO_DEVICE    #READ_FROM_DEVICE ¦   #WRITE_TO_DEVICE ®   #HALO_EXCHANGE ¶   #GATHER_INNER_DATA ¼                 $                                            ¸                      #FIELD_TYPE                       @               @                        '¸                    #DEFINED_ON    #GRID    #INTERNAL a   #WHOLE b   #NUM_HALOS c   #HALO d   #DATA_ON_DEVICE i   #READ_FROM_DEVICE_C j   #READ_FROM_DEVICE_F r   #WRITE_TO_DEVICE_C z   #WRITE_TO_DEVICE_F                  $                                                                       $                                                                #GRID_TYPE                       @              Ä                        '             /      #NAME 	   #OFFSET 
   #GLOBAL_NX    #GLOBAL_NY    #NX    #NY    #DX    #DY    #TMASK    #TMASK_DEVICE    #BOUNDARY_CONDITIONS    #SUBDOMAIN    #DECOMP !   #DX_T ,   #DY_T -   #DX_T_DEVICE .   #DY_T_DEVICE /   #DX_U 0   #DY_U 1   #DX_U_DEVICE 2   #DY_U_DEVICE 3   #DX_V 4   #DY_V 5   #DX_V_DEVICE 6   #DY_V_DEVICE 7   #DX_F 8   #DY_F 9   #DX_F_DEVICE :   #DY_F_DEVICE ;   #AREA_T <   #AREA_U =   #AREA_V >   #AREA_T_DEVICE ?   #AREA_U_DEVICE @   #AREA_V_DEVICE A   #GPHIU B   #GPHIV C   #GPHIF D   #GPHIU_DEVICE E   #GPHIV_DEVICE F   #GPHIF_DEVICE G   #XT H   #YT I   #XT_DEVICE J   #YT_DEVICE K   #GET_TMASK L   #DECOMPOSE O                 $                                       	                                 $                                       
                                $                                                                       $                                                                       $                                                                       $                                                                       $                                                     
                 $                                                      
               $                                                   (              	               &                   &                                                         $                                                         
       #C_PTR                      @                                     '                    #PTR                  D                                                                      $                                                                      p          p            p                                        $                                            0                     #SUBDOMAIN_TYPE                   @   @                                      '0                    #GLOBAL    #INTERNAL                                                                                     #REGION_TYPE                   @   @                                      '                    #NX    #NY    #XSTART    #XSTOP    #YSTART    #YSTOP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               #REGION_TYPE                  $                                       !     È       Ð              #DECOMPOSITION_TYPE "                  @   @              D                   "     'È              	      #GLOBAL_NX #   #GLOBAL_NY $   #NX %   #NY &   #NDOMAINS '   #MAX_WIDTH (   #MAX_HEIGHT )   #SUBDOMAINS *   #PROC_SUBDOMAINS +                                                        #                                                                        $                                                                       %                                                                       &                                                                       '                                                                       (                                                                       )                                                                     *                    0             #SUBDOMAIN_TYPE              &                                                                                              +            h              	               &                   &                                                       $                                      ,                            
            &                   &                                                       $                                      -            ø                
            &                   &                                                         $                                      .            X             #C_PTR                  $                                      /            `             #C_PTR                $                                      0            h                
            &                   &                                                       $                                      1            È                
            &                   &                                                         $                                      2            (             #C_PTR                  $                                      3            0             #C_PTR                $                                      4            8                
            &                   &                                                       $                                      5                            
            &                   &                                                         $                                      6            ø             #C_PTR                  $                                      7                          #C_PTR                $                                      8                            
            &                   &                                                       $                                      9            h                
            &                   &                                                         $                                      :            È             #C_PTR                  $                                      ;            Ð             #C_PTR                $                                      <            Ø                
            &                   &                                                       $                                      =            8                
            &                   &                                                       $                                      >                             
            &                   &                                                         $                                      ?            ø      !       #C_PTR                  $                                      @                   "       #C_PTR                  $                                      A                  #       #C_PTR                $                                      B                         $   
            &                   &                                                       $                                      C            p             %   
            &                   &                                                       $                                      D            Ð             &   
            &                   &                                                         $                                      E            0      '       #C_PTR                  $                                      F            8      (       #C_PTR                  $                                      G            @      )       #C_PTR                $                                      H            H             *   
            &                   &                                                       $                                      I            ¨             +   
            &                   &                                                         $                                      J                  ,       #C_PTR                  $                                      K                  -       #C_PTR    1         À    $                                    L             .     #GET_TMASK M   (         D   @                                    M                                       #SELF N             &                   &                                                     
                                          N                  #GRID_TYPE    1         À    $                                     O             /     #DECOMPOSE P   #         @     @                                     P                   #FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%PARALLEL_ABORT Q   #FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%PARALLEL_FINALISE S   #FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS T   #SELF Z   #DOMAINX [   #DOMAINY \   #NDOMAINS ]   #NDOMAINX ^   #NDOMAINY _   #HALO_WIDTH `   #        @                                           Q                    #MSG R             
                                         R                    1 #        @                                           S                     #        @                                           T                    #DECOMP U   #TMASK V   #PBC W   #HALO_DEPTHS X   #IERR Y             
  `                                        U     È              #DECOMPOSITION_TYPE "           
                                           V                    $             &                   &                                                     
                                           W                     
                                           X                    %   p          p            p                                     @                                        Y                      
                                         Z                   #GRID_TYPE              
  @                                        [                     
  @                                        \                     
 @                                        ]                     
 @                                        ^                     
 @                                        _                     
 @                                        `                         $                                       a                          #REGION_TYPE                  $                                       b            (              #REGION_TYPE                  $                                       c     @                         $                                       d            H       4             #HALO_TYPE e             &                                                          @   @                                 e     '4                    #NEEDS_UPDATE f   #SOURCE g   #DEST h                 $                                       f                                 $                                       g                          #REGION_TYPE                  $                                       h                          #REGION_TYPE                  $                                       i                  #         @    $                      d               j                    #FROM k   #TO l   #STARTX m   #STARTY n   #NX o   #NY p   #BLOCKING q                     
                                       k                    #C_PTR              
                                       l                    #C_PTR              
                                       m                      
                                       n                      
                                       o                      
                                       p                      
                                       q             #         @ @  $                      d               r                    #FROM s   #TO t   #STARTX u   #STARTY v   #NX w   #NY x   #BLOCKING y   	                   
                                        s                   #C_PTR              
                                       t                   
               &                   &                                                     
                                         u                     
                                         v                     
                                         w                     
                                         x                     
                                         y           #         @    $                      d               z                    #FROM {   #TO |   #STARTX }   #STARTY ~   #NX    #NY    #BLOCKING    
 ¨                 
                                       {                    #C_PTR              
                                       |                    #C_PTR              
                                       }                      
                                       ~                      
                                                             
                                                             
                                                    #         @    $                      d                                   #FROM    #TO    #STARTX    #STARTY    #NX    #NY    #BLOCKING     °                 
                                                           
              &                   &                                                     
                                                           #C_PTR              
                                                              
                                                              
                                                              
                                                              
                                                                  $                                            ¸                         $                                                   À       0             #TILE_TYPE              &                                                          @   @                                      '0                    #INTERNAL    #WHOLE                                                                                    #REGION_TYPE                                                                                   #REGION_TYPE               $                                                                  
            &                   &                                                         $                                                  h             #C_PTR    1         À    $                                                      #SET_DATA    %         @    @                                                               #SELF    #ARRAY               @                                            p              #R2D_FIELD                                                                          
 
              &                   &                                           1         À    $                                                      #GET_DATA    (         D   @                                                       	                
    #SELF              &                   &                                                      `                                            p              #R2D_FIELD    1         À    $                                                       #READ_HALO_FROM_DEVICE    #         @     @                                                        #READ_HALO_FROM_DEVICE%GLOBAL_SUM    #SELF    #COMM    #BLOCKING    #        @                                                               #VAR              
 @                                            
                 
 `                                            p              #R2D_FIELD              
                                                                
  @                                                   1         À    $                                                  	     #WRITE_HALO_TO_DEVICE     #         @     @                                                         #WRITE_HALO_TO_DEVICE%GLOBAL_SUM ¡   #SELF £   #COMM ¤   #BLOCKING ¥   #        @                                           ¡                    #VAR ¢             
 @                                       ¢     
                 
 `                                       £     p              #R2D_FIELD              
                                           ¤                     
  @                                        ¥           1         À    $                                     ¦             
     #READ_FROM_DEVICE §   #         @     @                                     §                    #SELF ¨   #STARTX ©   #STARTY ª   #NX «   #NY ¬   #BLOCKING ­              `                                       ¨     p              #R2D_FIELD              
 @                                        ©                     
 @                                        ª                     
 @                                        «                     
 @                                        ¬                     
 @                                        ­           1         À    $                                     ®                  #WRITE_TO_DEVICE ¯   #         @     @                                     ¯                    #SELF °   #STARTX ±   #STARTY ²   #NX ³   #NY ´   #BLOCKING µ              `                                       °     p              #R2D_FIELD              
 @                                        ±                     
 @                                        ²                     
 @                                        ³                     
 @                                        ´                     
 @                                        µ           1         À    $                                     ¶                  #HALO_EXCHANGE ·   #         @     @                                     ·                   #HALO_EXCHANGE%GLOBAL_SUM ¸   #SELF º   #DEPTH »   #        @                                           ¸                    #VAR ¹             
 @                                       ¹     
                 
 `                                       º     p              #R2D_FIELD              
                                           »           1         À    $                                     ¼                  #GATHER_INNER_DATA ½   #         @     @                                     ½                   #GATHER_INNER_DATA%GET_NUM_RANKS ¾   #SELF ¿   #GLOBAL_DATA À   %        @                                         ¾                                      
                                          ¿     p             #R2D_FIELD                                                     À                   
               &                   &                                                                                                 Á                                                         &         @    @                                   Â     p                     #GRID Ã   #GRID_POINTS Ä   #DO_TILE Å   #INIT_GLOBAL_DATA Æ   #R2D_FIELD              
                                           Ã                  #GRID_TYPE              
  @                                        Ä                     
 @                                        Å                     
 @                                       Æ                   
              &                   &                                           #         @                                            Ç                    #NEIGHBOURS È   #CURRENT É   #BORN Ê   #DIE Ë             
D @                                        È     p              #R2D_FIELD              
D @                                        É     p              #R2D_FIELD              
D @                                        Ê     p              #R2D_FIELD              
D @                                        Ë     p              #R2D_FIELD    #         @                                            Ì                   #READ_FROM_DEVICE%C_PTR Í   #FROM Ï   #TO Ð   #STARTX Ñ   #STARTY Ò   #NX Ó   #NY Ô   #BLOCKING Õ                                                  @                                  Í     '                    #PTR Î                 D                                      Î                             
                                          Ï                   #READ_FROM_DEVICE%C_PTR Í             
                                         Ð                   
               &                   &                                                     
                                           Ñ                     
                                           Ò                     
                                           Ó                     
                                           Ô                     
                                           Õ           #         @                                            Ö                    #I ×   #J Ø   #NEIGHBOURS Ù   #C Ú             
                                           ×                     
                                           Ø                     D                                          Ù                   
               &                   &                                                     
                                           Ú                   
              &                   &                                           #         @                                            Û                    #I Ü   #J Ý   #BORN Þ   #CURRENT ß   #NEIGHBOURS à             
                                           Ü                     
                                           Ý                     D                                          Þ                   
               &                   &                                                     
                                           ß                   
              &                   &                                                     
                                           à                   
              &                   &                                           #         @                                            á                    #I â   #J ã   #DIE ä   #CURRENT å   #NEIGHBOURS æ             
                                           â                     
                                           ã                     D                                          ä                   
               &                   &                                                     
                                           å                   
              &                   &                                                     
                                           æ                   
              &                   &                                           #         @                                            ç                    #I è   #J é   #CURRENT ê   #DIE ë   #BORN ì             
                                           è                     
                                           é                     D                                          ê                   
 	              &                   &                                                     
                                           ë                   
 
             &                   &                                                     
                                           ì                   
              &                   &                                                  8      fn#fn    Ø   H   J   FIELD_MOD        H   J   KIND_PARAMS_MOD $   h  >      R2D_FIELD+FIELD_MOD /   ¦  h   a   R2D_FIELD%FIELD_TYPE+FIELD_MOD %           FIELD_TYPE+FIELD_MOD 0   $  P   a   FIELD_TYPE%DEFINED_ON+FIELD_MOD *   t  g   a   FIELD_TYPE%GRID+FIELD_MOD #   Û  Õ      GRID_TYPE+GRID_MOD (   °  P   a   GRID_TYPE%NAME+GRID_MOD *      P   a   GRID_TYPE%OFFSET+GRID_MOD -   P  P   a   GRID_TYPE%GLOBAL_NX+GRID_MOD -      P   a   GRID_TYPE%GLOBAL_NY+GRID_MOD &   ð  P   a   GRID_TYPE%NX+GRID_MOD &   @	  P   a   GRID_TYPE%NY+GRID_MOD &   	  P   a   GRID_TYPE%DX+GRID_MOD &   à	  P   a   GRID_TYPE%DY+GRID_MOD )   0
  ´   a   GRID_TYPE%TMASK+GRID_MOD 0   ä
  c   a   GRID_TYPE%TMASK_DEVICE+GRID_MOD $   G  a       C_PTR+ISO_C_BINDING ,   ¨  P   %   C_PTR%PTR+ISO_C_BINDING=PTR 7   ø  ¤   a   GRID_TYPE%BOUNDARY_CONDITIONS+GRID_MOD -     l   a   GRID_TYPE%SUBDOMAIN+GRID_MOD 1     r      SUBDOMAIN_TYPE+DECOMPOSITION_MOD 8   z  i   a   SUBDOMAIN_TYPE%GLOBAL+DECOMPOSITION_MOD '   ã        REGION_TYPE+REGION_MOD *   y  P   a   REGION_TYPE%NX+REGION_MOD *   É  P   a   REGION_TYPE%NY+REGION_MOD .     P   a   REGION_TYPE%XSTART+REGION_MOD -   i  P   a   REGION_TYPE%XSTOP+REGION_MOD .   ¹  P   a   REGION_TYPE%YSTART+REGION_MOD -   	  P   a   REGION_TYPE%YSTOP+REGION_MOD :   Y  i   a   SUBDOMAIN_TYPE%INTERNAL+DECOMPOSITION_MOD *   Â  p   a   GRID_TYPE%DECOMP+GRID_MOD 5   2  Ø      DECOMPOSITION_TYPE+DECOMPOSITION_MOD ?   
  P   a   DECOMPOSITION_TYPE%GLOBAL_NX+DECOMPOSITION_MOD ?   Z  P   a   DECOMPOSITION_TYPE%GLOBAL_NY+DECOMPOSITION_MOD 8   ª  P   a   DECOMPOSITION_TYPE%NX+DECOMPOSITION_MOD 8   ú  P   a   DECOMPOSITION_TYPE%NY+DECOMPOSITION_MOD >   J  P   a   DECOMPOSITION_TYPE%NDOMAINS+DECOMPOSITION_MOD ?     P   a   DECOMPOSITION_TYPE%MAX_WIDTH+DECOMPOSITION_MOD @   ê  P   a   DECOMPOSITION_TYPE%MAX_HEIGHT+DECOMPOSITION_MOD @   :  °   a   DECOMPOSITION_TYPE%SUBDOMAINS+DECOMPOSITION_MOD E   ê  ´   a   DECOMPOSITION_TYPE%PROC_SUBDOMAINS+DECOMPOSITION_MOD (     ´   a   GRID_TYPE%DX_T+GRID_MOD (   R  ´   a   GRID_TYPE%DY_T+GRID_MOD /     c   a   GRID_TYPE%DX_T_DEVICE+GRID_MOD /   i  c   a   GRID_TYPE%DY_T_DEVICE+GRID_MOD (   Ì  ´   a   GRID_TYPE%DX_U+GRID_MOD (     ´   a   GRID_TYPE%DY_U+GRID_MOD /   4  c   a   GRID_TYPE%DX_U_DEVICE+GRID_MOD /     c   a   GRID_TYPE%DY_U_DEVICE+GRID_MOD (   ú  ´   a   GRID_TYPE%DX_V+GRID_MOD (   ®  ´   a   GRID_TYPE%DY_V+GRID_MOD /   b  c   a   GRID_TYPE%DX_V_DEVICE+GRID_MOD /   Å  c   a   GRID_TYPE%DY_V_DEVICE+GRID_MOD (   (  ´   a   GRID_TYPE%DX_F+GRID_MOD (   Ü  ´   a   GRID_TYPE%DY_F+GRID_MOD /     c   a   GRID_TYPE%DX_F_DEVICE+GRID_MOD /   ó  c   a   GRID_TYPE%DY_F_DEVICE+GRID_MOD *   V  ´   a   GRID_TYPE%AREA_T+GRID_MOD *   
  ´   a   GRID_TYPE%AREA_U+GRID_MOD *   ¾  ´   a   GRID_TYPE%AREA_V+GRID_MOD 1   r   c   a   GRID_TYPE%AREA_T_DEVICE+GRID_MOD 1   Õ   c   a   GRID_TYPE%AREA_U_DEVICE+GRID_MOD 1   8!  c   a   GRID_TYPE%AREA_V_DEVICE+GRID_MOD )   !  ´   a   GRID_TYPE%GPHIU+GRID_MOD )   O"  ´   a   GRID_TYPE%GPHIV+GRID_MOD )   #  ´   a   GRID_TYPE%GPHIF+GRID_MOD 0   ·#  c   a   GRID_TYPE%GPHIU_DEVICE+GRID_MOD 0   $  c   a   GRID_TYPE%GPHIV_DEVICE+GRID_MOD 0   }$  c   a   GRID_TYPE%GPHIF_DEVICE+GRID_MOD &   à$  ´   a   GRID_TYPE%XT+GRID_MOD &   %  ´   a   GRID_TYPE%YT+GRID_MOD -   H&  c   a   GRID_TYPE%XT_DEVICE+GRID_MOD -   «&  c   a   GRID_TYPE%YT_DEVICE+GRID_MOD -   '  _   a   GRID_TYPE%GET_TMASK+GRID_MOD M   m'  Æ      FIELD_MOD^R2D_FIELD_CONSTRUCTOR%GET_TMASK+GRID_MOD=GET_TMASK H   3(  _   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%GET_TMASK%SELF+GRID_MOD -   (  _   a   GRID_TYPE%DECOMPOSE+GRID_MOD M   ñ(  f     FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE+GRID_MOD=DECOMPOSE k   W*  Y      FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%PARALLEL_ABORT+PARALLEL_UTILS_MOD=PARALLEL_ABORT `   °*  T   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%PARALLEL_ABORT%MSG+PARALLEL_UTILS_MOD q   +  P      FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%PARALLEL_FINALISE+PARALLEL_UTILS_MOD=PARALLEL_FINALISE a   T+        FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS+PARALLEL_COMMS_MOD=MAP_COMMS ^   ß+  h   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS%DECOMP+PARALLEL_COMMS_MOD ]   G,  ¬   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS%TMASK+PARALLEL_COMMS_MOD [   ó,  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS%PBC+PARALLEL_COMMS_MOD c   ;-     a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS%HALO_DEPTHS+PARALLEL_COMMS_MOD \   ×-  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%MAP_COMMS%IERR+PARALLEL_COMMS_MOD H   .  _   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%SELF+GRID_MOD K   ~.  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%DOMAINX+GRID_MOD K   Æ.  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%DOMAINY+GRID_MOD L   /  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%NDOMAINS+GRID_MOD L   V/  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%NDOMAINX+GRID_MOD L   /  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%NDOMAINY+GRID_MOD N   æ/  H   a   FIELD_MOD^R2D_FIELD_CONSTRUCTOR%DECOMPOSE%HALO_WIDTH+GRID_MOD .   .0  i   a   FIELD_TYPE%INTERNAL+FIELD_MOD +   0  i   a   FIELD_TYPE%WHOLE+FIELD_MOD /    1  P   a   FIELD_TYPE%NUM_HALOS+FIELD_MOD *   P1  «   a   FIELD_TYPE%HALO+FIELD_MOD #   û1        HALO_TYPE+HALO_MOD 0   {2  P   a   HALO_TYPE%NEEDS_UPDATE+HALO_MOD *   Ë2  i   a   HALO_TYPE%SOURCE+HALO_MOD (   43  i   a   HALO_TYPE%DEST+HALO_MOD 4   3  P   a   FIELD_TYPE%DATA_ON_DEVICE+FIELD_MOD K   í3  ¢      FIELD_TYPE%READ_FROM_DEVICE_C+FIELD_MOD=READ_FROM_DEVICE_C =   4  [   a   FIELD_TYPE%READ_FROM_DEVICE_C%FROM+FIELD_MOD ;   ê4  [   a   FIELD_TYPE%READ_FROM_DEVICE_C%TO+FIELD_MOD ?   E5  H   a   FIELD_TYPE%READ_FROM_DEVICE_C%STARTX+FIELD_MOD ?   5  H   a   FIELD_TYPE%READ_FROM_DEVICE_C%STARTY+FIELD_MOD ;   Õ5  H   a   FIELD_TYPE%READ_FROM_DEVICE_C%NX+FIELD_MOD ;   6  H   a   FIELD_TYPE%READ_FROM_DEVICE_C%NY+FIELD_MOD A   e6  H   a   FIELD_TYPE%READ_FROM_DEVICE_C%BLOCKING+FIELD_MOD K   ­6  ¢      FIELD_TYPE%READ_FROM_DEVICE_F+FIELD_MOD=READ_FROM_DEVICE_F =   O7  [   a   FIELD_TYPE%READ_FROM_DEVICE_F%FROM+FIELD_MOD ;   ª7  ¬   a   FIELD_TYPE%READ_FROM_DEVICE_F%TO+FIELD_MOD ?   V8  H   a   FIELD_TYPE%READ_FROM_DEVICE_F%STARTX+FIELD_MOD ?   8  H   a   FIELD_TYPE%READ_FROM_DEVICE_F%STARTY+FIELD_MOD ;   æ8  H   a   FIELD_TYPE%READ_FROM_DEVICE_F%NX+FIELD_MOD ;   .9  H   a   FIELD_TYPE%READ_FROM_DEVICE_F%NY+FIELD_MOD A   v9  H   a   FIELD_TYPE%READ_FROM_DEVICE_F%BLOCKING+FIELD_MOD I   ¾9  ¢      FIELD_TYPE%WRITE_TO_DEVICE_C+FIELD_MOD=WRITE_TO_DEVICE_C <   `:  [   a   FIELD_TYPE%WRITE_TO_DEVICE_C%FROM+FIELD_MOD :   »:  [   a   FIELD_TYPE%WRITE_TO_DEVICE_C%TO+FIELD_MOD >   ;  H   a   FIELD_TYPE%WRITE_TO_DEVICE_C%STARTX+FIELD_MOD >   ^;  H   a   FIELD_TYPE%WRITE_TO_DEVICE_C%STARTY+FIELD_MOD :   ¦;  H   a   FIELD_TYPE%WRITE_TO_DEVICE_C%NX+FIELD_MOD :   î;  H   a   FIELD_TYPE%WRITE_TO_DEVICE_C%NY+FIELD_MOD @   6<  H   a   FIELD_TYPE%WRITE_TO_DEVICE_C%BLOCKING+FIELD_MOD I   ~<  ¢      FIELD_TYPE%WRITE_TO_DEVICE_F+FIELD_MOD=WRITE_TO_DEVICE_F <    =  ¬   a   FIELD_TYPE%WRITE_TO_DEVICE_F%FROM+FIELD_MOD :   Ì=  [   a   FIELD_TYPE%WRITE_TO_DEVICE_F%TO+FIELD_MOD >   '>  H   a   FIELD_TYPE%WRITE_TO_DEVICE_F%STARTX+FIELD_MOD >   o>  H   a   FIELD_TYPE%WRITE_TO_DEVICE_F%STARTY+FIELD_MOD :   ·>  H   a   FIELD_TYPE%WRITE_TO_DEVICE_F%NX+FIELD_MOD :   ÿ>  H   a   FIELD_TYPE%WRITE_TO_DEVICE_F%NY+FIELD_MOD @   G?  H   a   FIELD_TYPE%WRITE_TO_DEVICE_F%BLOCKING+FIELD_MOD +   ?  P   a   R2D_FIELD%NTILES+FIELD_MOD )   ß?  «   a   R2D_FIELD%TILE+FIELD_MOD #   @  q      TILE_TYPE+TILE_MOD ,   û@  i   a   TILE_TYPE%INTERNAL+TILE_MOD )   dA  i   a   TILE_TYPE%WHOLE+TILE_MOD )   ÍA  ´   a   R2D_FIELD%DATA+FIELD_MOD /   B  c   a   R2D_FIELD%DEVICE_PTR+FIELD_MOD -   äB  ^   a   R2D_FIELD%SET_DATA+FIELD_MOD #   BC  m      SET_DATA+FIELD_MOD (   ¯C  _   a   SET_DATA%SELF+FIELD_MOD )   D  ¬   a   SET_DATA%ARRAY+FIELD_MOD -   ºD  ^   a   R2D_FIELD%GET_DATA+FIELD_MOD #   E  Æ      GET_DATA+FIELD_MOD (   ÞE  _   a   GET_DATA%SELF+FIELD_MOD :   =F  k   a   R2D_FIELD%READ_HALO_FROM_DEVICE+FIELD_MOD 0   ¨F        READ_HALO_FROM_DEVICE+FIELD_MOD O   @G  Y      READ_HALO_FROM_DEVICE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM H   G  H   a   READ_HALO_FROM_DEVICE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD 5   áG  _   a   READ_HALO_FROM_DEVICE%SELF+FIELD_MOD 5   @H  H   a   READ_HALO_FROM_DEVICE%COMM+FIELD_MOD 9   H  H   a   READ_HALO_FROM_DEVICE%BLOCKING+FIELD_MOD 9   ÐH  j   a   R2D_FIELD%WRITE_HALO_TO_DEVICE+FIELD_MOD /   :I        WRITE_HALO_TO_DEVICE+FIELD_MOD N   ÑI  Y      WRITE_HALO_TO_DEVICE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM G   *J  H   a   WRITE_HALO_TO_DEVICE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD 4   rJ  _   a   WRITE_HALO_TO_DEVICE%SELF+FIELD_MOD 4   ÑJ  H   a   WRITE_HALO_TO_DEVICE%COMM+FIELD_MOD 8   K  H   a   WRITE_HALO_TO_DEVICE%BLOCKING+FIELD_MOD 5   aK  f   a   R2D_FIELD%READ_FROM_DEVICE+FIELD_MOD +   ÇK        READ_FROM_DEVICE+FIELD_MOD 0   WL  _   a   READ_FROM_DEVICE%SELF+FIELD_MOD 2   ¶L  H   a   READ_FROM_DEVICE%STARTX+FIELD_MOD 2   þL  H   a   READ_FROM_DEVICE%STARTY+FIELD_MOD .   FM  H   a   READ_FROM_DEVICE%NX+FIELD_MOD .   M  H   a   READ_FROM_DEVICE%NY+FIELD_MOD 4   ÖM  H   a   READ_FROM_DEVICE%BLOCKING+FIELD_MOD 4   N  e   a   R2D_FIELD%WRITE_TO_DEVICE+FIELD_MOD *   N        WRITE_TO_DEVICE+FIELD_MOD /   O  _   a   WRITE_TO_DEVICE%SELF+FIELD_MOD 1   rO  H   a   WRITE_TO_DEVICE%STARTX+FIELD_MOD 1   ºO  H   a   WRITE_TO_DEVICE%STARTY+FIELD_MOD -   P  H   a   WRITE_TO_DEVICE%NX+FIELD_MOD -   JP  H   a   WRITE_TO_DEVICE%NY+FIELD_MOD 3   P  H   a   WRITE_TO_DEVICE%BLOCKING+FIELD_MOD 2   ÚP  c   a   R2D_FIELD%HALO_EXCHANGE+FIELD_MOD (   =Q        HALO_EXCHANGE+FIELD_MOD G   ÀQ  Y      HALO_EXCHANGE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM @   R  H   a   HALO_EXCHANGE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD -   aR  _   a   HALO_EXCHANGE%SELF+FIELD_MOD .   ÀR  H   a   HALO_EXCHANGE%DEPTH+FIELD_MOD 6   S  g   a   R2D_FIELD%GATHER_INNER_DATA+FIELD_MOD ,   oS        GATHER_INNER_DATA+FIELD_MOD Q   ÿS  X      GATHER_INNER_DATA%GET_NUM_RANKS+PARALLEL_UTILS_MOD=GET_NUM_RANKS 1   WT  _   a   GATHER_INNER_DATA%SELF+FIELD_MOD 8   ¶T  ¬   a   GATHER_INNER_DATA%GLOBAL_DATA+FIELD_MOD &   bU  x       GO_WP+KIND_PARAMS_MOD 0   ÚU  ¥      R2D_FIELD_CONSTRUCTOR+FIELD_MOD 5   V  _   a   R2D_FIELD_CONSTRUCTOR%GRID+FIELD_MOD <   ÞV  H   a   R2D_FIELD_CONSTRUCTOR%GRID_POINTS+FIELD_MOD 8   &W  H   a   R2D_FIELD_CONSTRUCTOR%DO_TILE+FIELD_MOD A   nW  ¬   a   R2D_FIELD_CONSTRUCTOR%INIT_GLOBAL_DATA+FIELD_MOD    X         INVOKE_COMPUTE *   X  _   a   INVOKE_COMPUTE%NEIGHBOURS '   ùX  _   a   INVOKE_COMPUTE%CURRENT $   XY  _   a   INVOKE_COMPUTE%BORN #   ·Y  _   a   INVOKE_COMPUTE%DIE !   Z  Ð       READ_FROM_DEVICE 5   æZ  a      READ_FROM_DEVICE%C_PTR+ISO_C_BINDING =   G[  P   %   READ_FROM_DEVICE%C_PTR%PTR+ISO_C_BINDING=PTR &   [  l   a   READ_FROM_DEVICE%FROM $   \  ¬   a   READ_FROM_DEVICE%TO (   ¯\  H   a   READ_FROM_DEVICE%STARTX (   ÷\  H   a   READ_FROM_DEVICE%STARTY $   ?]  H   a   READ_FROM_DEVICE%NX $   ]  H   a   READ_FROM_DEVICE%NY *   Ï]  H   a   READ_FROM_DEVICE%BLOCKING &   ^  u       COUNT_NEIGHBOURS_CODE (   ^  H   a   COUNT_NEIGHBOURS_CODE%I (   Ô^  H   a   COUNT_NEIGHBOURS_CODE%J 1   _  ¬   a   COUNT_NEIGHBOURS_CODE%NEIGHBOURS (   È_  ¬   a   COUNT_NEIGHBOURS_CODE%C "   t`         COMPUTE_BORN_CODE $   ù`  H   a   COMPUTE_BORN_CODE%I $   Aa  H   a   COMPUTE_BORN_CODE%J '   a  ¬   a   COMPUTE_BORN_CODE%BORN *   5b  ¬   a   COMPUTE_BORN_CODE%CURRENT -   áb  ¬   a   COMPUTE_BORN_CODE%NEIGHBOURS !   c         COMPUTE_DIE_CODE #   d  H   a   COMPUTE_DIE_CODE%I #   Yd  H   a   COMPUTE_DIE_CODE%J %   ¡d  ¬   a   COMPUTE_DIE_CODE%DIE )   Me  ¬   a   COMPUTE_DIE_CODE%CURRENT ,   ùe  ¬   a   COMPUTE_DIE_CODE%NEIGHBOURS    ¥f  ~       COMBINE_CODE    #g  H   a   COMBINE_CODE%I    kg  H   a   COMBINE_CODE%J %   ³g  ¬   a   COMBINE_CODE%CURRENT !   _h  ¬   a   COMBINE_CODE%DIE "   i  ¬   a   COMBINE_CODE%BORN 