  Ä^  Ï   k820309    é          2021.10.0   ¶uÈf                                                                                                          
       read_config_mod.f90 READ_CONFIG_MOD              READ_CONFIG #         @                                                              #READ_CONFIG%GET_NUM_RANKS À   #READ_CONFIG%GET_RANK Á   #READ_CONFIG%MAP_COMMS Â   #READ_CONFIG%PARALLEL_FINALISE È   #READ_CONFIG%PARALLEL_ABORT É   #READ_CONFIG%DECOMPOSITION_TYPE    #READ_CONFIG%SUBDOMAIN_TYPE    #READ_CONFIG%C_PTR    #READ_CONFIG%REGION_TYPE    #READ_CONFIG%TILE_TYPE    #READ_CONFIG%R2D_FIELD    #READ_CONFIG%GRID_TYPE     #GRID Ë   #INITIAL Ì   #TIME_STEPS Í                                                                                                                                                                      @   @              D                        'È              	      #GLOBAL_NX    #GLOBAL_NY    #NX    #NY    #NDOMAINS    #MAX_WIDTH    #MAX_HEIGHT 	   #SUBDOMAINS 
   #PROC_SUBDOMAINS                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    	                                                                     
                    0             #READ_CONFIG%SUBDOMAIN_TYPE              &                                                         @   @                                      '0                    #GLOBAL    #INTERNAL                                                                                    #READ_CONFIG%REGION_TYPE                   @   @                                      '                    #NX    #NY    #XSTART    #XSTOP    #YSTART    #YSTOP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              #READ_CONFIG%REGION_TYPE                                                                   h              	               &                   &                                                         À   @                                     '                    #PTR                  D                                                                       @   @                                      '0                    #INTERNAL    #WHOLE                                                                                    #READ_CONFIG%REGION_TYPE                                                                                   #READ_CONFIG%REGION_TYPE                       @               À                       'p                   #FIELD_TYPE    #NTILES    #TILE    #DATA    #DEVICE_PTR    #SET_DATA    #GET_DATA    #READ_HALO_FROM_DEVICE    #WRITE_HALO_TO_DEVICE    #READ_FROM_DEVICE ¥   #WRITE_TO_DEVICE ­   #HALO_EXCHANGE µ   #GATHER_INNER_DATA »                 $                                            ¸                      #READ_CONFIG%FIELD_TYPE                       @               D                        '¸                    #DEFINED_ON    #GRID    #INTERNAL c   #WHOLE d   #NUM_HALOS e   #HALO f   #DATA_ON_DEVICE k   #READ_FROM_DEVICE_C l   #READ_FROM_DEVICE_F t   #WRITE_TO_DEVICE_C |   #WRITE_TO_DEVICE_F                  $                                                                       $                                                                #READ_CONFIG%GRID_TYPE                        @               À                         '             /      #NAME !   #OFFSET "   #GLOBAL_NX #   #GLOBAL_NY $   #NX %   #NY &   #DX '   #DY (   #TMASK )   #TMASK_DEVICE *   #BOUNDARY_CONDITIONS +   #SUBDOMAIN ,   #DECOMP -   #DX_T .   #DY_T /   #DX_T_DEVICE 0   #DY_T_DEVICE 1   #DX_U 2   #DY_U 3   #DX_U_DEVICE 4   #DY_U_DEVICE 5   #DX_V 6   #DY_V 7   #DX_V_DEVICE 8   #DY_V_DEVICE 9   #DX_F :   #DY_F ;   #DX_F_DEVICE <   #DY_F_DEVICE =   #AREA_T >   #AREA_U ?   #AREA_V @   #AREA_T_DEVICE A   #AREA_U_DEVICE B   #AREA_V_DEVICE C   #GPHIU D   #GPHIV E   #GPHIF F   #GPHIU_DEVICE G   #GPHIV_DEVICE H   #GPHIF_DEVICE I   #XT J   #YT K   #XT_DEVICE L   #YT_DEVICE M   #GET_TMASK N   #DECOMPOSE Q                 $                                       !                                 $                                       "                                $                                       #                                $                                       $                                $                                       %                                $                                       &                                $                                      '               
                 $                                      (                
               $                                       )            (              	               &                   &                                                         $                                      *                   
       #READ_CONFIG%C_PTR                  $                                       +                               p          p            p                                        $                                       ,     0                     #READ_CONFIG%SUBDOMAIN_TYPE                  $                                       -     È       Ð              #READ_CONFIG%DECOMPOSITION_TYPE                $                                      .                            
            &                   &                                                       $                                      /            ø                
            &                   &                                                         $                                      0            X             #READ_CONFIG%C_PTR                  $                                      1            `             #READ_CONFIG%C_PTR                $                                      2            h                
            &                   &                                                       $                                      3            È                
            &                   &                                                         $                                      4            (             #READ_CONFIG%C_PTR                  $                                      5            0             #READ_CONFIG%C_PTR                $                                      6            8                
            &                   &                                                       $                                      7                            
            &                   &                                                         $                                      8            ø             #READ_CONFIG%C_PTR                  $                                      9                          #READ_CONFIG%C_PTR                $                                      :                            
            &                   &                                                       $                                      ;            h                
            &                   &                                                         $                                      <            È             #READ_CONFIG%C_PTR                  $                                      =            Ð             #READ_CONFIG%C_PTR                $                                      >            Ø                
            &                   &                                                       $                                      ?            8                
            &                   &                                                       $                                      @                             
            &                   &                                                         $                                      A            ø      !       #READ_CONFIG%C_PTR                  $                                      B                   "       #READ_CONFIG%C_PTR                  $                                      C                  #       #READ_CONFIG%C_PTR                $                                      D                         $   
            &                   &                                                       $                                      E            p             %   
            &                   &                                                       $                                      F            Ð             &   
            &                   &                                                         $                                      G            0      '       #READ_CONFIG%C_PTR                  $                                      H            8      (       #READ_CONFIG%C_PTR                  $                                      I            @      )       #READ_CONFIG%C_PTR                $                                      J            H             *   
            &                   &                                                       $                                      K            ¨             +   
            &                   &                                                         $                                      L                  ,       #READ_CONFIG%C_PTR                  $                                      M                  -       #READ_CONFIG%C_PTR    1         À    $                                    N             .     #GET_TMASK O   (         D   @                                    O                                       #SELF P             &                   &                                                     
                                          P                  #GRID_TYPE     1         À    $                                     Q             /     #DECOMPOSE R   #         @     @                                     R                   #READ_CONFIG%DECOMPOSE%MAP_COMMS S   #READ_CONFIG%DECOMPOSE%PARALLEL_FINALISE Y   #READ_CONFIG%DECOMPOSE%PARALLEL_ABORT Z   #SELF \   #DOMAINX ]   #DOMAINY ^   #NDOMAINS _   #NDOMAINX `   #NDOMAINY a   #HALO_WIDTH b   #        @                                           S                    #DECOMP T   #TMASK U   #PBC V   #HALO_DEPTHS W   #IERR X             
  `                                        T     È              #READ_CONFIG%DECOMPOSITION_TYPE            
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
 @                                        b                         $                                       c                          #READ_CONFIG%REGION_TYPE                  $                                       d            (              #READ_CONFIG%REGION_TYPE                  $                                       e     @                         $                                       f            H       4             #READ_CONFIG%HALO_TYPE g             &                                                          @   @                                 g     '4                    #NEEDS_UPDATE h   #SOURCE i   #DEST j                 $                                       h                                 $                                       i                          #READ_CONFIG%REGION_TYPE                  $                                       j                          #READ_CONFIG%REGION_TYPE                  $                                       k                  #         @    $                      d               l                    #FROM m   #TO n   #STARTX o   #STARTY p   #NX q   #NY r   #BLOCKING s                     
                                       m                    #READ_CONFIG%C_PTR              
                                       n                    #READ_CONFIG%C_PTR              
                                       o                      
                                       p                      
                                       q                      
                                       r                      
                                       s             #         @    $                      d               t                    #FROM u   #TO v   #STARTX w   #STARTY x   #NX y   #NY z   #BLOCKING {   	                   
                                        u                   #READ_CONFIG%C_PTR              
                                       v                   
               &                   &                                                     
                                         w                     
                                         x                     
                                         y                     
                                         z                     
                                         {           #         @    $                      d               |                    #FROM }   #TO ~   #STARTX    #STARTY    #NX    #NY    #BLOCKING    
 ¨                 
                                       }                    #READ_CONFIG%C_PTR              
                                       ~                    #READ_CONFIG%C_PTR              
                                                             
                                                             
                                                             
                                                             
                                                    #         @    $                      d                                   #FROM    #TO    #STARTX    #STARTY    #NX    #NY    #BLOCKING     °                 
                                                           
              &                   &                                                     
                                                           #READ_CONFIG%C_PTR              
                                                              
                                                              
                                                              
                                                              
                                                                  $                                            ¸                         $                                                   À       0             #READ_CONFIG%TILE_TYPE              &                                                      $                                                                  
            &                   &                                                         $                                                  h             #READ_CONFIG%C_PTR    1         À    $                                                      #SET_DATA    %         @    @                                                               #SELF    #ARRAY               @                                            p              #R2D_FIELD                                                                          
 
              &                   &                                           1         À    $                                                      #GET_DATA    (         D   @                                                       	                
    #SELF              &                   &                                                      `                                            p              #R2D_FIELD    1         À    $                                                       #READ_HALO_FROM_DEVICE    #         @     @                                                        #READ_CONFIG%READ_HALO_FROM_DEVICE%GLOBAL_SUM    #SELF    #COMM    #BLOCKING    #        @                                                               #VAR              
                                              
                 
 `                                            p              #R2D_FIELD              
                                                                
  @                                                   1         À    $                                                  	     #WRITE_HALO_TO_DEVICE    #         @     @                                                        #READ_CONFIG%WRITE_HALO_TO_DEVICE%GLOBAL_SUM     #SELF ¢   #COMM £   #BLOCKING ¤   #        @                                                                #VAR ¡             
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
 @                                        ´           1         À    $                                     µ                  #HALO_EXCHANGE ¶   #         @     @                                     ¶                   #READ_CONFIG%HALO_EXCHANGE%GLOBAL_SUM ·   #SELF ¹   #DEPTH º   #        @                                           ·                    #VAR ¸             
                                         ¸     
                 
 `                                       ¹     p              #R2D_FIELD              
                                           º           1         À    $                                     »                  #GATHER_INNER_DATA ¼   #         @     @                                     ¼                   #READ_CONFIG%GATHER_INNER_DATA%GET_NUM_RANKS ½   #SELF ¾   #GLOBAL_DATA ¿   %        @                                         ½                                      
                                          ¾     p             #R2D_FIELD                                                     ¿                   
               &                   &                                           %        @                                         À                            %        @                                         Á                            #        @                                           Â                    #DECOMP Ã   #TMASK Ä   #PBC Å   #HALO_DEPTHS Æ   #IERR Ç             
  `                                        Ã     È              #READ_CONFIG%DECOMPOSITION_TYPE            
                                           Ä                    $             &                   &                                                     
                                           Å                     
                                           Æ                    %   p          p            p                                     @                                        Ç            #        @                                           È                     #        @                                           É                    #MSG Ê             
                                         Ê                    1           D `                                        Ë                   #READ_CONFIG%GRID_TYPE               D                                          Ì     p              #READ_CONFIG%R2D_FIELD              D                                          Í                   ,      fn#fn %   Ì      b   uapp(READ_CONFIG_MOD    è   l      READ_CONFIG T   T  Ø      READ_CONFIG%DECOMPOSITION_TYPE+DECOMPOSITION_MOD=DECOMPOSITION_TYPE K   ,  P   a   READ_CONFIG%DECOMPOSITION_TYPE%GLOBAL_NX+DECOMPOSITION_MOD K   |  P   a   READ_CONFIG%DECOMPOSITION_TYPE%GLOBAL_NY+DECOMPOSITION_MOD D   Ì  P   a   READ_CONFIG%DECOMPOSITION_TYPE%NX+DECOMPOSITION_MOD D     P   a   READ_CONFIG%DECOMPOSITION_TYPE%NY+DECOMPOSITION_MOD J   l  P   a   READ_CONFIG%DECOMPOSITION_TYPE%NDOMAINS+DECOMPOSITION_MOD K   ¼  P   a   READ_CONFIG%DECOMPOSITION_TYPE%MAX_WIDTH+DECOMPOSITION_MOD L     P   a   READ_CONFIG%DECOMPOSITION_TYPE%MAX_HEIGHT+DECOMPOSITION_MOD L   \  ¼   a   READ_CONFIG%DECOMPOSITION_TYPE%SUBDOMAINS+DECOMPOSITION_MOD L     r      READ_CONFIG%SUBDOMAIN_TYPE+DECOMPOSITION_MOD=SUBDOMAIN_TYPE D     u   a   READ_CONFIG%SUBDOMAIN_TYPE%GLOBAL+DECOMPOSITION_MOD ?   ÿ        READ_CONFIG%REGION_TYPE+REGION_MOD=REGION_TYPE 6     P   a   READ_CONFIG%REGION_TYPE%NX+REGION_MOD 6   å  P   a   READ_CONFIG%REGION_TYPE%NY+REGION_MOD :   5	  P   a   READ_CONFIG%REGION_TYPE%XSTART+REGION_MOD 9   	  P   a   READ_CONFIG%REGION_TYPE%XSTOP+REGION_MOD :   Õ	  P   a   READ_CONFIG%REGION_TYPE%YSTART+REGION_MOD 9   %
  P   a   READ_CONFIG%REGION_TYPE%YSTOP+REGION_MOD F   u
  u   a   READ_CONFIG%SUBDOMAIN_TYPE%INTERNAL+DECOMPOSITION_MOD Q   ê
  ´   a   READ_CONFIG%DECOMPOSITION_TYPE%PROC_SUBDOMAINS+DECOMPOSITION_MOD 6     a      READ_CONFIG%C_PTR+ISO_C_BINDING=C_PTR 8   ÿ  P   %   READ_CONFIG%C_PTR%PTR+ISO_C_BINDING=PTR 9   O  q      READ_CONFIG%TILE_TYPE+TILE_MOD=TILE_TYPE 8   À  u   a   READ_CONFIG%TILE_TYPE%INTERNAL+TILE_MOD 5   5  u   a   READ_CONFIG%TILE_TYPE%WHOLE+TILE_MOD 0   ª  >     READ_CONFIG%R2D_FIELD+FIELD_MOD ;   è  t   a   READ_CONFIG%R2D_FIELD%FIELD_TYPE+FIELD_MOD <   \       READ_CONFIG%FIELD_TYPE+FIELD_MOD=FIELD_TYPE <   r  P   a   READ_CONFIG%FIELD_TYPE%DEFINED_ON+FIELD_MOD 6   Â  s   a   READ_CONFIG%FIELD_TYPE%GRID+FIELD_MOD /   5  Õ     READ_CONFIG%GRID_TYPE+GRID_MOD 4   
  P   a   READ_CONFIG%GRID_TYPE%NAME+GRID_MOD 6   Z  P   a   READ_CONFIG%GRID_TYPE%OFFSET+GRID_MOD 9   ª  P   a   READ_CONFIG%GRID_TYPE%GLOBAL_NX+GRID_MOD 9   ú  P   a   READ_CONFIG%GRID_TYPE%GLOBAL_NY+GRID_MOD 2   J  P   a   READ_CONFIG%GRID_TYPE%NX+GRID_MOD 2     P   a   READ_CONFIG%GRID_TYPE%NY+GRID_MOD 2   ê  P   a   READ_CONFIG%GRID_TYPE%DX+GRID_MOD 2   :  P   a   READ_CONFIG%GRID_TYPE%DY+GRID_MOD 5     ´   a   READ_CONFIG%GRID_TYPE%TMASK+GRID_MOD <   >  o   a   READ_CONFIG%GRID_TYPE%TMASK_DEVICE+GRID_MOD C   ­  ¤   a   READ_CONFIG%GRID_TYPE%BOUNDARY_CONDITIONS+GRID_MOD 9   Q  x   a   READ_CONFIG%GRID_TYPE%SUBDOMAIN+GRID_MOD 6   É  |   a   READ_CONFIG%GRID_TYPE%DECOMP+GRID_MOD 4   E  ´   a   READ_CONFIG%GRID_TYPE%DX_T+GRID_MOD 4   ù  ´   a   READ_CONFIG%GRID_TYPE%DY_T+GRID_MOD ;   ­  o   a   READ_CONFIG%GRID_TYPE%DX_T_DEVICE+GRID_MOD ;     o   a   READ_CONFIG%GRID_TYPE%DY_T_DEVICE+GRID_MOD 4     ´   a   READ_CONFIG%GRID_TYPE%DX_U+GRID_MOD 4   ?  ´   a   READ_CONFIG%GRID_TYPE%DY_U+GRID_MOD ;   ó  o   a   READ_CONFIG%GRID_TYPE%DX_U_DEVICE+GRID_MOD ;   b  o   a   READ_CONFIG%GRID_TYPE%DY_U_DEVICE+GRID_MOD 4   Ñ  ´   a   READ_CONFIG%GRID_TYPE%DX_V+GRID_MOD 4     ´   a   READ_CONFIG%GRID_TYPE%DY_V+GRID_MOD ;   9  o   a   READ_CONFIG%GRID_TYPE%DX_V_DEVICE+GRID_MOD ;   ¨  o   a   READ_CONFIG%GRID_TYPE%DY_V_DEVICE+GRID_MOD 4      ´   a   READ_CONFIG%GRID_TYPE%DX_F+GRID_MOD 4   Ë   ´   a   READ_CONFIG%GRID_TYPE%DY_F+GRID_MOD ;   !  o   a   READ_CONFIG%GRID_TYPE%DX_F_DEVICE+GRID_MOD ;   î!  o   a   READ_CONFIG%GRID_TYPE%DY_F_DEVICE+GRID_MOD 6   ]"  ´   a   READ_CONFIG%GRID_TYPE%AREA_T+GRID_MOD 6   #  ´   a   READ_CONFIG%GRID_TYPE%AREA_U+GRID_MOD 6   Å#  ´   a   READ_CONFIG%GRID_TYPE%AREA_V+GRID_MOD =   y$  o   a   READ_CONFIG%GRID_TYPE%AREA_T_DEVICE+GRID_MOD =   è$  o   a   READ_CONFIG%GRID_TYPE%AREA_U_DEVICE+GRID_MOD =   W%  o   a   READ_CONFIG%GRID_TYPE%AREA_V_DEVICE+GRID_MOD 5   Æ%  ´   a   READ_CONFIG%GRID_TYPE%GPHIU+GRID_MOD 5   z&  ´   a   READ_CONFIG%GRID_TYPE%GPHIV+GRID_MOD 5   .'  ´   a   READ_CONFIG%GRID_TYPE%GPHIF+GRID_MOD <   â'  o   a   READ_CONFIG%GRID_TYPE%GPHIU_DEVICE+GRID_MOD <   Q(  o   a   READ_CONFIG%GRID_TYPE%GPHIV_DEVICE+GRID_MOD <   À(  o   a   READ_CONFIG%GRID_TYPE%GPHIF_DEVICE+GRID_MOD 2   /)  ´   a   READ_CONFIG%GRID_TYPE%XT+GRID_MOD 2   ã)  ´   a   READ_CONFIG%GRID_TYPE%YT+GRID_MOD 9   *  o   a   READ_CONFIG%GRID_TYPE%XT_DEVICE+GRID_MOD 9   +  o   a   READ_CONFIG%GRID_TYPE%YT_DEVICE+GRID_MOD 9   u+  _   a   READ_CONFIG%GRID_TYPE%GET_TMASK+GRID_MOD 9   Ô+  Æ      READ_CONFIG%GET_TMASK+GRID_MOD=GET_TMASK 4   ,  _   a   READ_CONFIG%GET_TMASK%SELF+GRID_MOD 9   ù,  _   a   READ_CONFIG%GRID_TYPE%DECOMPOSE+GRID_MOD 9   X-  *     READ_CONFIG%DECOMPOSE+GRID_MOD=DECOMPOSE M   .        READ_CONFIG%DECOMPOSE%MAP_COMMS+PARALLEL_COMMS_MOD=MAP_COMMS J   /  t   a   READ_CONFIG%DECOMPOSE%MAP_COMMS%DECOMP+PARALLEL_COMMS_MOD I   /  ¬   a   READ_CONFIG%DECOMPOSE%MAP_COMMS%TMASK+PARALLEL_COMMS_MOD G   -0  H   a   READ_CONFIG%DECOMPOSE%MAP_COMMS%PBC+PARALLEL_COMMS_MOD O   u0     a   READ_CONFIG%DECOMPOSE%MAP_COMMS%HALO_DEPTHS+PARALLEL_COMMS_MOD H   1  H   a   READ_CONFIG%DECOMPOSE%MAP_COMMS%IERR+PARALLEL_COMMS_MOD ]   Y1  P      READ_CONFIG%DECOMPOSE%PARALLEL_FINALISE+PARALLEL_UTILS_MOD=PARALLEL_FINALISE W   ©1  Y      READ_CONFIG%DECOMPOSE%PARALLEL_ABORT+PARALLEL_UTILS_MOD=PARALLEL_ABORT L   2  T   a   READ_CONFIG%DECOMPOSE%PARALLEL_ABORT%MSG+PARALLEL_UTILS_MOD 4   V2  _   a   READ_CONFIG%DECOMPOSE%SELF+GRID_MOD 7   µ2  H   a   READ_CONFIG%DECOMPOSE%DOMAINX+GRID_MOD 7   ý2  H   a   READ_CONFIG%DECOMPOSE%DOMAINY+GRID_MOD 8   E3  H   a   READ_CONFIG%DECOMPOSE%NDOMAINS+GRID_MOD 8   3  H   a   READ_CONFIG%DECOMPOSE%NDOMAINX+GRID_MOD 8   Õ3  H   a   READ_CONFIG%DECOMPOSE%NDOMAINY+GRID_MOD :   4  H   a   READ_CONFIG%DECOMPOSE%HALO_WIDTH+GRID_MOD :   e4  u   a   READ_CONFIG%FIELD_TYPE%INTERNAL+FIELD_MOD 7   Ú4  u   a   READ_CONFIG%FIELD_TYPE%WHOLE+FIELD_MOD ;   O5  P   a   READ_CONFIG%FIELD_TYPE%NUM_HALOS+FIELD_MOD 6   5  ·   a   READ_CONFIG%FIELD_TYPE%HALO+FIELD_MOD 9   V6        READ_CONFIG%HALO_TYPE+HALO_MOD=HALO_TYPE <   Ö6  P   a   READ_CONFIG%HALO_TYPE%NEEDS_UPDATE+HALO_MOD 6   &7  u   a   READ_CONFIG%HALO_TYPE%SOURCE+HALO_MOD 4   7  u   a   READ_CONFIG%HALO_TYPE%DEST+HALO_MOD @   8  P   a   READ_CONFIG%FIELD_TYPE%DATA_ON_DEVICE+FIELD_MOD W   `8  ¢      READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C+FIELD_MOD=READ_FROM_DEVICE_C I   9  g   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%FROM+FIELD_MOD G   i9  g   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%TO+FIELD_MOD K   Ð9  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%STARTX+FIELD_MOD K   :  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%STARTY+FIELD_MOD G   `:  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%NX+FIELD_MOD G   ¨:  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%NY+FIELD_MOD M   ð:  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_C%BLOCKING+FIELD_MOD W   8;  ¢      READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F+FIELD_MOD=READ_FROM_DEVICE_F I   Ú;  g   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%FROM+FIELD_MOD G   A<  ¬   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%TO+FIELD_MOD K   í<  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%STARTX+FIELD_MOD K   5=  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%STARTY+FIELD_MOD G   }=  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%NX+FIELD_MOD G   Å=  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%NY+FIELD_MOD M   >  H   a   READ_CONFIG%FIELD_TYPE%READ_FROM_DEVICE_F%BLOCKING+FIELD_MOD U   U>  ¢      READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C+FIELD_MOD=WRITE_TO_DEVICE_C H   ÷>  g   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%FROM+FIELD_MOD F   ^?  g   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%TO+FIELD_MOD J   Å?  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%STARTX+FIELD_MOD J   @  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%STARTY+FIELD_MOD F   U@  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%NX+FIELD_MOD F   @  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%NY+FIELD_MOD L   å@  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_C%BLOCKING+FIELD_MOD U   -A  ¢      READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F+FIELD_MOD=WRITE_TO_DEVICE_F H   ÏA  ¬   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%FROM+FIELD_MOD F   {B  g   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%TO+FIELD_MOD J   âB  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%STARTX+FIELD_MOD J   *C  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%STARTY+FIELD_MOD F   rC  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%NX+FIELD_MOD F   ºC  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%NY+FIELD_MOD L   D  H   a   READ_CONFIG%FIELD_TYPE%WRITE_TO_DEVICE_F%BLOCKING+FIELD_MOD 7   JD  P   a   READ_CONFIG%R2D_FIELD%NTILES+FIELD_MOD 5   D  ·   a   READ_CONFIG%R2D_FIELD%TILE+FIELD_MOD 5   QE  ´   a   READ_CONFIG%R2D_FIELD%DATA+FIELD_MOD ;   F  o   a   READ_CONFIG%R2D_FIELD%DEVICE_PTR+FIELD_MOD 9   tF  ^   a   READ_CONFIG%R2D_FIELD%SET_DATA+FIELD_MOD 8   ÒF  m      READ_CONFIG%SET_DATA+FIELD_MOD=SET_DATA 4   ?G  _   a   READ_CONFIG%SET_DATA%SELF+FIELD_MOD 5   G  ¬   a   READ_CONFIG%SET_DATA%ARRAY+FIELD_MOD 9   JH  ^   a   READ_CONFIG%R2D_FIELD%GET_DATA+FIELD_MOD 8   ¨H  Æ      READ_CONFIG%GET_DATA+FIELD_MOD=GET_DATA 4   nI  _   a   READ_CONFIG%GET_DATA%SELF+FIELD_MOD F   ÍI  k   a   READ_CONFIG%R2D_FIELD%READ_HALO_FROM_DEVICE+FIELD_MOD R   8J  ¤      READ_CONFIG%READ_HALO_FROM_DEVICE+FIELD_MOD=READ_HALO_FROM_DEVICE [   ÜJ  Y      READ_CONFIG%READ_HALO_FROM_DEVICE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM T   5K  H   a   READ_CONFIG%READ_HALO_FROM_DEVICE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD A   }K  _   a   READ_CONFIG%READ_HALO_FROM_DEVICE%SELF+FIELD_MOD A   ÜK  H   a   READ_CONFIG%READ_HALO_FROM_DEVICE%COMM+FIELD_MOD E   $L  H   a   READ_CONFIG%READ_HALO_FROM_DEVICE%BLOCKING+FIELD_MOD E   lL  j   a   READ_CONFIG%R2D_FIELD%WRITE_HALO_TO_DEVICE+FIELD_MOD P   ÖL  £      READ_CONFIG%WRITE_HALO_TO_DEVICE+FIELD_MOD=WRITE_HALO_TO_DEVICE Z   yM  Y      READ_CONFIG%WRITE_HALO_TO_DEVICE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM S   ÒM  H   a   READ_CONFIG%WRITE_HALO_TO_DEVICE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD @   N  _   a   READ_CONFIG%WRITE_HALO_TO_DEVICE%SELF+FIELD_MOD @   yN  H   a   READ_CONFIG%WRITE_HALO_TO_DEVICE%COMM+FIELD_MOD D   ÁN  H   a   READ_CONFIG%WRITE_HALO_TO_DEVICE%BLOCKING+FIELD_MOD A   	O  f   a   READ_CONFIG%R2D_FIELD%READ_FROM_DEVICE+FIELD_MOD H   oO        READ_CONFIG%READ_FROM_DEVICE+FIELD_MOD=READ_FROM_DEVICE <   ÿO  _   a   READ_CONFIG%READ_FROM_DEVICE%SELF+FIELD_MOD >   ^P  H   a   READ_CONFIG%READ_FROM_DEVICE%STARTX+FIELD_MOD >   ¦P  H   a   READ_CONFIG%READ_FROM_DEVICE%STARTY+FIELD_MOD :   îP  H   a   READ_CONFIG%READ_FROM_DEVICE%NX+FIELD_MOD :   6Q  H   a   READ_CONFIG%READ_FROM_DEVICE%NY+FIELD_MOD @   ~Q  H   a   READ_CONFIG%READ_FROM_DEVICE%BLOCKING+FIELD_MOD @   ÆQ  e   a   READ_CONFIG%R2D_FIELD%WRITE_TO_DEVICE+FIELD_MOD F   +R        READ_CONFIG%WRITE_TO_DEVICE+FIELD_MOD=WRITE_TO_DEVICE ;   »R  _   a   READ_CONFIG%WRITE_TO_DEVICE%SELF+FIELD_MOD =   S  H   a   READ_CONFIG%WRITE_TO_DEVICE%STARTX+FIELD_MOD =   bS  H   a   READ_CONFIG%WRITE_TO_DEVICE%STARTY+FIELD_MOD 9   ªS  H   a   READ_CONFIG%WRITE_TO_DEVICE%NX+FIELD_MOD 9   òS  H   a   READ_CONFIG%WRITE_TO_DEVICE%NY+FIELD_MOD ?   :T  H   a   READ_CONFIG%WRITE_TO_DEVICE%BLOCKING+FIELD_MOD >   T  c   a   READ_CONFIG%R2D_FIELD%HALO_EXCHANGE+FIELD_MOD B   åT        READ_CONFIG%HALO_EXCHANGE+FIELD_MOD=HALO_EXCHANGE S   tU  Y      READ_CONFIG%HALO_EXCHANGE%GLOBAL_SUM+PARALLEL_UTILS_MOD=GLOBAL_SUM L   ÍU  H   a   READ_CONFIG%HALO_EXCHANGE%GLOBAL_SUM%VAR+PARALLEL_UTILS_MOD 9   V  _   a   READ_CONFIG%HALO_EXCHANGE%SELF+FIELD_MOD :   tV  H   a   READ_CONFIG%HALO_EXCHANGE%DEPTH+FIELD_MOD B   ¼V  g   a   READ_CONFIG%R2D_FIELD%GATHER_INNER_DATA+FIELD_MOD J   #W        READ_CONFIG%GATHER_INNER_DATA+FIELD_MOD=GATHER_INNER_DATA ]   ¿W  X      READ_CONFIG%GATHER_INNER_DATA%GET_NUM_RANKS+PARALLEL_UTILS_MOD=GET_NUM_RANKS =   X  _   a   READ_CONFIG%GATHER_INNER_DATA%SELF+FIELD_MOD D   vX  ¬   a   READ_CONFIG%GATHER_INNER_DATA%GLOBAL_DATA+FIELD_MOD K   "Y  X      READ_CONFIG%GET_NUM_RANKS+PARALLEL_UTILS_MOD=GET_NUM_RANKS A   zY  X      READ_CONFIG%GET_RANK+PARALLEL_UTILS_MOD=GET_RANK C   ÒY        READ_CONFIG%MAP_COMMS+PARALLEL_COMMS_MOD=MAP_COMMS @   ]Z  t   a   READ_CONFIG%MAP_COMMS%DECOMP+PARALLEL_COMMS_MOD ?   ÑZ  ¬   a   READ_CONFIG%MAP_COMMS%TMASK+PARALLEL_COMMS_MOD =   }[  H   a   READ_CONFIG%MAP_COMMS%PBC+PARALLEL_COMMS_MOD E   Å[     a   READ_CONFIG%MAP_COMMS%HALO_DEPTHS+PARALLEL_COMMS_MOD >   a\  H   a   READ_CONFIG%MAP_COMMS%IERR+PARALLEL_COMMS_MOD S   ©\  P      READ_CONFIG%PARALLEL_FINALISE+PARALLEL_UTILS_MOD=PARALLEL_FINALISE M   ù\  Y      READ_CONFIG%PARALLEL_ABORT+PARALLEL_UTILS_MOD=PARALLEL_ABORT B   R]  T   a   READ_CONFIG%PARALLEL_ABORT%MSG+PARALLEL_UTILS_MOD !   ¦]  k   a   READ_CONFIG%GRID $   ^  k   a   READ_CONFIG%INITIAL '   |^  H   a   READ_CONFIG%TIME_STEPS 