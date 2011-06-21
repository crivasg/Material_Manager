################################################################################
# This file provide a set of tools useful to update materials associated to comps
# As soon as materials are assigned to components with the rigth name these tools
# enable user to load and update existing materials from database
# for  NASTRAN PAMCRASH2G LS-DYNA & RADIOSS44BLK templates
# Author: Arnaud Stauffer September 10th 2004
################################################################################
catch { namespace delete ::ACF::Material}
namespace eval ::ACF::Material:: {
    Source [hm_info -appinfo SPECIFIEDPATH hm_scripts_dir]/hmtools.tcl;
}
#
################################################################################
#                     Card Images for solver
################################################################################
################################################################################
# Proc to find the "Card Image" of a Ls-Dyna Material
################################################################################
proc ::ACF::Material::CardFromlsdyna {matid} {
    set index 1;
    set nMax [ hm_attributeindexmax mats $matid];
    set card ""
    while {([Null card] && $index<=$nMax)} {
        switch -- [hm_attributeindexidentifier mats $matid $index ] {
            2319 {set card "NOMAT"}
            2320 {set card "MATL1"}
            2318 {set card "MATL2"}
            2321 {set card "MATL3"}
            2322 {set card "MATL5"}
            2323 {set card "MATL6"}
            2324 {set card "MATL7"}
            2325 {set card "MATL9"}
            2330 {set card "MATL12"}
            2331 {set card "MATL14"}
            2332 {set card "MATL15"}
            2333 {set card "MATL18"}
            2334 {set card "MATL19"}
            2335 {set card "MATL20"}
            2336 {set card "MATL22"}
            2337 {set card "MATL24"}
            2338 {set card "MATL26"}
            2339 {set card "MATL27"}
            2340 {set card "MATL28"}
            2341 {set card "MATL29"}
            2342 {set card "MATL30"}
            2343 {set card "MATL31"}
            2344 {set card "MATL32"}
            2345 {set card "MATL34"}
            2346 {set card "MATL37"}
            2347 {set card "MATL39"}
            2348 {set card "MATL52"}
            2349 {set card "MATL53"}
            2350 {set card "MATL57"}
            2351 {set card "MATL59"}
            2352 {set card "MATL62"}
            2353 {set card "MATL63"}
            2354 {set card "MATL64"}
            2355 {set card "MATL66"}
            2356 {set card "MATL69"}
            2357 {set card "SB_MAT"}
            2358 {set card "MATL71"}
            2373 {set card "MATL77"}
            2378 {set card "MATL81"}
            2379 {set card "SDMAT1"}
            2380 {set card "SDMAT2"}
            2381 {set card "SDMAT3"}
            2382 {set card "SDMAT4"}
            2383 {set card "SDMAT5"}
            2384 {set card "SDMAT6"}
            2385 {set card "SDMAT7"}
            2455 {set card "MATL33"}
            2456 {set card "MATL35"}
            2457 {set card "MATL41"}
            2472 {set card "MATL77B"}
            2496 {set card "MATL126"}
            2508 {set card "MATL73"}
            2525 {set card "MATL100"}
            2539 {set card "MATL90"}
            2552 {set card "MATL88"}
            3211 {set card "MATL89"}
            2586 {set card "MATL96"}
            2600 {set card "MATL83"}
            2624 {set card "MATLT1"}
            2630 {set card "MATLT2"}
            2644 {set card "MATLT6"}
            2647 {set card "MATL103"}
            2684 {set card "MATL72"}
            2766 {set card "MATL68"}
            2816 {set card "MATL124"}
            3001 {set card "MATL67"}
            3014 {set card "MATL10"}
            4208 {set card "MATL74"}
            4248 {set card "MATL123"}
            4255 {set card "MATL58"}
            4264 {set card "MATL98"}
            4373 {set card "MATL76"}
            4393 {set card "MATL8"}
            4486 {set card "MATL99"}
            4559 {set card "MATL146"}
            4562 {set card "MATL121"}
            4567 {set card "MATL119"}
            4574 {set card "MATL154"}
            4580 {set card "MATL163"}
            4584 {set card "MATL179"}
            4585 {set card "MATL181"}
            4589 {set card "MATL140"}
            4614 {set card "MATL180"}
            4744 {set card "MATL97"}
            4761 {set card "MATL148"}
            5044 {set card "MATL36"}
            5079 {set card "MATL145"}
        }
        incr index;
    }   
    return $card;
}
#
################################################################################
# Proc to find the "Card Image" of a Radioss Material
################################################################################
proc ::ACF::Material::CardFromradioss {matid} {
    set index 1;
    set nMax [ hm_attributeindexmax mats $matid];
    set card ""
    while {([Null card] && $index<=$nMax)} {
        switch -- [hm_attributeindexidentifier mats $matid $index ] {
            2325 {set card "M0_VOID"}
            2320 {set card "M1_ELAST"}
            2321 {set card "M2_PLAS_JOHNS_ZERIL"}
            8055 {set card "LAW2_ID"}
            5031 {set card "M3_HYDPLA"}
            2332 {set card "M4_HYD_JCOOK"}
            5032 {set card "M6_HYD_VISC"}
            2322 {set card "MLAW10"}
            2351 {set card "M14_COMPSO"}
            2318 {set card "M19_FABRI"}
            2331 {set card "M21_DPRAG"}
            2337 {set card "M22_DAMA"}
            2333 {set card "MLAW23"}
            5033 {set card "M24_CONC"}
            2336 {set card "M25_COMPSH"}
            2346 {set card "M27_PLAS_BRIT"}
            2338 {set card "M28_HONEYCOMB"}
            5034 {set card "M32_HILL"}
            2349 {set card "M33_FOAM_PLAS"}
            2323 {set card "M34_BOLTZMAN"}
            2352 {set card "M35_FOAM_VISC"}
            2341 {set card "M36_PLAS_TAB"}
            5035 {set card "M38_VISC_TAB"}
            5323 {set card "M40_KELVINMAX"}
            7002 {set card "M42_OGDEN"}
            8071 {set card "M43_HILL_TAB"}
            8072 {set card "MLAW50"}
            8073 {set card "M52_GURSON"}
            6000 {set card "MLAW53"}
            6001 {set card "M54_PREDIT"}
        }
        incr index;
    }
    return $card;
}
#
################################################################################
# Proc to find the "Card Image" of an Abaqus Material
################################################################################
proc ::ACF::Material::CardFromabaqus {matid} {
    set index 1;
    set nMax [ hm_attributeindexmax mats $matid];
    set card ""
    while {([Null card] && $index<=$nMax)} {
        switch -- [hm_attributeindexidentifier mats $matid $index ] {
            109 {set card "ABAQUS_MATERIAL"}
            871 {set card "CONNECTOR_BEHAVIOR"}
            4031 {set card "GASKET_MATERIAL"}
        }
        incr index;
    }
    return $card;
}
#
################################################################################
# Proc to find the "Card Image" of a Pamcrash2G Material
################################################################################
proc ::ACF::Material::CardFrompamcrash2g {matid} {
    set index 1;
    set nMax [ hm_attributeindexmax mats $matid];
    set card ""
    while {([Null card] && $index<=$nMax)} {
        switch -- [hm_attributeindexidentifier mats $matid $index ] {
            413 {set card "MAT_2D"}
            122 {set card "MAT_3D"}
            610 {set card "MAT_1D"}
            3329 {set card "MAT_LINK"}
            1990 {set card "MAT_SECURE"}
        }
        incr index;
    }
    return $card;
}
#
################################################################################
# Proc to update the cards of one material for one solver (pam lsdyna & radioss)
################################################################################
proc ::ACF::Material::updatemats {newmatid oldmatid template solvername} {
    set solver [hm_attributeindexsolver  mats $newmatid 1];
    set LAW [::ACF::Material::CardFrom$solvername $newmatid];
    set nMax [ hm_attributeindexmax mats $newmatid ];
    *clearmark mats 1
    *createmark mats 1 $oldmatid
    for {set index 1} {$index <= $nMax} {incr index} {
        catch {set attributes [hm_attributeindexidentifier mats $newmatid $index]};
        if {![Null attributes]} {
            set type [hm_attributeindextype mats $newmatid $index];
            set status [hm_attributeindexstatus mats $newmatid $index];
            set value [hm_attributeindexvalue mats $newmatid $index];
            set behavior [hm_attributeindexbehavior mats $newmatid $index];
            *dictionaryload materials 1 $template $LAW
            switch -- $type {
                1 {*attributeupdateint mats $oldmatid $attributes $solver $status $behavior $value;}
                2 {*attributeupdatedouble mats $oldmatid $attributes $solver $status $behavior $value;}
                #3 {*attributeupdatestring mats $oldmatid $solver $status $behavior $value;}
                6 {*attributeupdateentity mats $oldmatid $attributes $solver $status $behavior curve $value;}
                
            }
        }
    }
}
proc ::ACF::Material::deletecard {cardlist attribs cards} {
    foreach cardid $cardlist {
        set nMax [ hm_attributeindexmax card $cardid]
        for  {set i 1} {$i<=$nMax} {incr i} {
            foreach att $attribs\
                    card $cards {
                        if {[hm_attributeindexidentifier cards $cardid $i]==$att} {
                            catch {*carddelete "$card"}
                            return;
                        }                    
            }            
        }
    }
    return;
}
#
################################################################################
# Proc to update materials attributes for Pam2G LS-Dyna & Radioss44blk
################################################################################
proc ::ACF::Material::updatesolver {solver materialid matpath} {
    set templatedir [hm_info -appinfo SPECIFIEDPATH TEMPLATES_DIR]/feoutput;
    set importedmats {}       
    foreach matid $materialid {
        switch -- $solver {
            abaqus {
                set translator "#abaqus/abaqus"
                set template $templatedir/abaqus/standard.3d
            }
            lsdyna {
                set translator "#ls-dyna/dynakey"
                set template $templatedir/ls-dyna/dyna.key
                ::ACF::Material::deletecard [hm_entitylist cards id] 3102 ReadMe}
            
            radioss {
                set translator "#radioss/radiossblk"
                set template $templatedir/radioss/radioss44.blk
                ::ACF::Material::deletecard [hm_entitylist cards id] 5015 "HEADER_CARD"}
            pamcrash2g {
                set translator "#pamcrash2G/pamcrash2G"
                set template $templatedir/pamcrash2g/pam2g2004
                ::ACF::Material::deletecard [hm_entitylist cards id] {3145 8012 8014} {"IMPORTED_MODEL_DOC" "ATTR_UnsuppCards" "ATTR_UnsupportedCards"}}
        }
        if {[file exists $matpath]==1} {
            if {$solver == "nastran"} {
                ::ACF::Material::deletecard [hm_entitylist cards id] {3242 3243 3244} {OMIT_BEGIN_BULK OMIT_END_BULK OMIT_CEND}
                lappend importedmats [::ACF::Material::importnastranmat [hm_getcollectorname mats $matid] $matpath]
            } else  {
                *feinput $translator "$matpath" 0 0 -0.01 1 0
                ::ACF::Material::updatemats [hm_entitymaxid mats] $matid $template $solver
                lappend importedmats [hm_entitymaxid mats]
            }
        }
    }
    return $importedmats;
}
#
#
################################################################################
# Procs to update materials attributes for nastran wrt unsupported TABLES1 cards
################################################################################
proc ::ACF::Material::creatematifnotexist {materialname} {
    set matnames {}
    *clearmark mats 1
    *createmark mats 1 "all"
    foreach mat [hm_getmark mats 1] {
        lappend matnames [hm_getcollectorname mats $mat]
    }
    if {[lsearch -exact $matnames $materialname]==-1} {
        *collectorcreate materials $materialname "" 1
    }
}
#
proc ::ACF::Material::findindex {list keyword} {
    set lines [llength $list];
    set index 0;
    while {$index<$lines && [string last $keyword [lindex $list $index]]<0 && [string first $keyword [lindex $list $index]]<0} {
        incr index
    }
    if {[string last $keyword [lindex $list $index]]<0 && [string first $keyword [lindex $list $index]]<0} {
        return -1
    } else  {
        return $index
    }
}
#
proc ::ACF::Material::linearmat {MATS TABLES  END MAT1 linelist} {
    set j [::ACF::Material::findindex $linelist $TABLES]
    set k [::ACF::Material::findindex $linelist $END]
    if {$j>-1 && $k>-1} {
        set linelist [lreplace $linelist $j $k];
    }
    set i [::ACF::Material::findindex $linelist $MATS]
    set index [::ACF::Material::findindex $linelist $MAT1]
    if {$i<$index} {
        if {$index>0} {
            set out [lreplace $linelist 0 [incr index -1]];
        }
    } else {
        set out [lreplace $linelist i end];
    }
    return $out;
}
#
proc ::ACF::Material::importnastranmat {materialname materialpath} {
    #Get templates and translator
    set template [hm_info -appinfo SPECIFIEDPATH TEMPLATES_DIR]/feoutput/nastran/general
    set translator "#nastran/nastran"
    *createmark cards 1 "OMIT_BEGIN_BULK" "OMIT_END_BULK" "OMIT_CEND"
    if {[hm_marklength cards 1]>0} {
        *deletemark cards 1
    }
    #Create a plot for table curve
    set plots [hm_entitylist plots name]
    if {[lsearch -exact $plots "Nastran_Functions"]==-1} {
        *xyplotcreate "Nastran_Functions" ""
    } else  {
        *xyplotsetcurrent "Nastran_Functions"
    }
####################################################################
#     #Read materials from file or import classical MAT1, MAT2, etc.
####################################################################
    if {[file exists $materialpath]==1} {
        set channel [open $materialpath r]
        set lines [split [read $channel] \n]
        close $channel
        set LAW "MAT1"
        set MATS1_index [::ACF::Material::findindex $lines MATS1];
################################################################################
# IF the material is a MATS1 material hence:
#  1)write the TABLES1 card as a cuve, if it exists and inport curve
#  2)write the linear isotropic card MAT1 associate in a file and import it
##
################################################################################
        if {$MATS1_index} {
            set TABLES1_index [::ACF::Material::findindex $lines TABLES1]
            set ENDT_index [::ACF::Material::findindex $lines ENDT]
            if {$TABLES1_index && $ENDT_index && $ENDT_index>=$TABLES1_index} {
                set MATS1_line [lindex $lines $MATS1_index]
                set TABLES1_line [lindex $lines $TABLES1_index]
                set ENDT_line [lindex $lines $ENDT_index]
                set xyplot [lreplace [lreplace $lines $ENDT_index end] 0 $TABLES1_index];
                #set currentdir [join [split [hm_info -appinfo CURRENTWORKINGDIR] \\] /];
                set channel [open [pwd]/tempmat.txt w+]
                puts $channel [format "%7s %32s" "XYDATA," "$materialname"]
                foreach l $xyplot {
                    set size [llength $l]
                    incr size -2
                    for  {set i 1} {$i<$size} {incr i 2} {
                        set j [expr {$i+1}]
                        puts $channel [format "%16f%1s %16f" [lindex $l $i] "," [lindex $l $j]]
                    }
                    puts $channel [format "%16f%1s %16f" [lindex $l $size] "," [lindex [split [lindex $l [incr size]] \+] 0]]
                }
                puts $channel [format "%7s" "ENDDATA"]
                close $channel
                *xyplotreadcurve [pwd]/tempmat.txt
                set CurveID [hm_entitymaxid curves]
                set curvenames [hm_entitylist curves name]
                if {[lsearch -exact $curvenames $materialname]==-1} {
                    catch {*renamecollector curves "curve1" "$materialname"}
                } else  {
                    set i 1
                    while {[lsearch -exact $curvenames $materialname$i]>=0} {
                        incr i
                    }
                    catch {*renamecollector curves "curve1" "$materialname$i"}
                }
                catch {file delete -force $currentdir/tempmat.txt}
            }
            set LAW "MATS1"
        }
        set channel [open [pwd]/tempmat.txt w+]
        set export [::ACF::Material::linearmat "MATS1" "TABLES1" "ENDT" "MAT1" $lines]
        foreach l $export {
            puts $channel $l
        }
        close $channel
##########################################
# IMPORT the material with nastran reader        
##########################################
        *feinput $translator "[pwd]/tempmat.txt" 0 0 -0.01 0 0
        set newmatid [hm_entitymaxid mats]
        set solver 1
        set nMax [ hm_attributeindexmax mats $newmatid ];
        for {set index 1} {$index <= $nMax} {incr index} {
            catch {set attributes [hm_attributeindexidentifier mats $newmatid $index]};
            if {![Null attributes] && $attributes !=146} {
                set type [hm_attributeindextype mats $newmatid $index];
                set status [hm_attributeindexstatus mats $newmatid $index];
                set value [hm_attributeindexvalue mats $newmatid $index];
                set behavior [hm_attributeindexbehavior mats $newmatid $index];
                *clearmark mats 1
                *createmark mats 1 $materialname
                set oldmatid [hm_getmark mats 1]
                *dictionaryload materials 1 $template $LAW
                switch -- $type {
                    1 {*attributeupdateint mats $oldmatid $attributes $solver $status $behavior $value;}
                    2 {*attributeupdatedouble mats $oldmatid $attributes $solver $status $behavior $value;}
                }
            }
        }
#############################################
#     Update MATS1 relative attributes
#############################################
        if {$MATS1_index} {
            set parameters {}
            for  {set j 31} {$j<64} {incr j 8} {
                set param {}
                for  {set i 1} {$i<=8} {incr i} {
                    set k [expr {$i+$j}]
                    set param $param[string index $MATS1_line $k]
                }
                lappend parameters $param
            }
            foreach val $parameters\
                    attrib {1812 1813 1814 1815 1816}\
                    type {double int int double double} {
                        if {[string compare $val "        "]>=0} {
                            *attributeupdate$type mats $oldmatid $attrib 1 1 0 $val;
                        } else  {
                            *attributeupdate$type mats $oldmatid $attrib 1 0 0 $val;
                        }
            }
            *attributeupdateentity mats $oldmatid 1810 1 1 0 curves $CurveID;
        }        
    }
    return [hm_entitymaxid mats]
}
#
################################################################################
#                   FIND A MAT IN THE DATABASE FROM ITS NAME
################################################################################
proc ::ACF::Material::findpath_indatabase { matname args } {
    set newdir $::Mat_Manager::matdir/
    set matname $matname.mtl
    set list1  [glob -path $newdir -nocomplain -type f *.mtl]
    if {[lsearch -exact $list1 $matname]>-1} {
        return $newdir/$matname;
    } else  {
        set list1 [glob -path $newdir -nocomplain -type d *]
        foreach i $list1 {
            #set i [file tail $i]
            set i $i/
            if {[lsearch -exact [glob -path $i -nocomplain -type f *.mtl] $i$matname]>-1} {
                return $i$matname;
            }
        }
    }   
}
#
#
################################################################################
#                             UPDATE ALL MATERIALS
################################################################################
proc ::ACF::Material::AllMaterialUpdate {solver} {
    hm_blockmessages 1
    switch -- $solver {
        abaqus {set code 0}
        nastran {set code 1}
        pamcrash2g {set code 18}
        radioss {set code 9}
        lsdyna {set code 9}
    }
    set materials [hm_entitylist mats name]
    set Matstodelete {}
    hm_usermessage "Update materials for $solver"
    foreach mat $materials {
        *dictionaryresetsolver materials "$mat" $code
        lappend Matstodelete [::ACF::Material::updatesolver $solver [hm_getentityvalue mats $mat "id" 0]\
                [::ACF::Material::findpath_indatabase $mat]]
    }

if {[llength [join $Matstodelete]]>0} {
    *clearmark mats 1
    eval *createmark mats 1 $Matstodelete
    *deletemark mats 1
}
hm_usermessage "Materials updated"
hm_blockmessages 0
}


