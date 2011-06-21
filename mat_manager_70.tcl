######Dialog Creation##################################################################
## This procedure creates the Dialog for the selection of the materials.
## It first checks whether Dialog window is already present or not.
## If present is raises the window else creates it with the given title
## according to the passed arguments.
#######################################################################################
catch { namespace delete ::Mat_Manager }

namespace eval ::Mat_Manager {
    Source [file join [file dirname [info script]] "MaterialUpdateTools_general.tcl"];
   variable dialog
   variable tre
   variable matdir
   variable matlist
   variable mat_to_add
   variable template
   variable solver
   variable unit_list
   variable new_dir_name
   variable new_mat_name
   variable listbox_name
   variable complist
   variable units
   variable matdir_base
   variable panel_name
   variable g_MatListWin
}

proc ::Mat_Manager::matlist_DialogCreate { title args } {

   if [winfo exists $::Mat_Manager::g_MatListWin] {
      switch -- [wm state $::Mat_Manager::g_MatListWin] {
         normal {
            # Raise a buried window
            raise $::Mat_Manager::g_MatListWin
         }
	 withdrawn -
	 iconic {
	    # Open and restore geometry
	    set badwindow [ catch { wm deiconify $::Mat_Manager::g_MatListWin } ]
	    if { $badwindow != 0 } {
	    	::Mat_Manager::main
	    }
	    catch {wm geometry $::Mat_Manager::g_MatListWin $::Mat_Manager::dialog(geo,$::Mat_Manager::g_MatListWin)}
	 }
      }
      return 0
   } else {
      CreateWindow [string trimleft $::Mat_Manager::g_MatListWin .] \
         windowtitle "$title" \
         geometry 300x400+176+176 \
         noGeometrySaving \
         cancelButton Close \
         acceptButton Rescan \
         acceptFunc "::Mat_Manager::Rescan" \
         defaultButton Cancel \
         propagate 1 \
         resize 0 \
         post;
      return 1
   }
}

######Rescan###########################################################################
## This procedure refreshes the material list
#######################################################################################
proc ::Mat_Manager::Rescan { args } {
   
   Tree $::Mat_Manager::tre clean
   ::Mat_Manager::create_recursive_list /
}

######List Creation####################################################################
## These procedures create the list of materials from the Database and lists
## the materials from the current model
#######################################################################################
proc ::Mat_Manager::create_recursive_list { name args } {

   set name1 "/Database"
   
   Tree $::Mat_Manager::tre add $name1 -popupfunc ::Mat_Manager::PopupDirDB
   ::Mat_Manager::create_recursive_list_database /
   
   set name1 "/Model"
   
   Tree $::Mat_Manager::tre add $name1 -popupfunc ::Mat_Manager::PopupMatMD
   ::Mat_Manager::create_list_model  
}

   proc ::Mat_Manager::create_recursive_list_database { name args } {

      set newdir $::Mat_Manager::matdir$name
      set list1 [glob -path $newdir -nocomplain -type d *]
      set name1 "/Database"
   
      foreach i $list1 {
         set i [file tail $i]
         Tree $::Mat_Manager::tre add $name1$name$i -popupfunc ::Mat_Manager::PopupDirDB1
         ::Mat_Manager::create_recursive_list_database ${name}$i/
      }

      set list1  [glob -path $newdir -nocomplain -type f *.mtl]
    
      foreach i $list1 {
         set i [file rootname $i]
         set i [file tail $i]
         Tree $::Mat_Manager::tre add $name1$name$i -image materials -popupfunc ::Mat_Manager::PopupMatDB
      }
   }

   proc ::Mat_Manager::create_list_model { args } {

      set name1 "/Model"   

      *createmark mats 1 "all"
      set ::Mat_Manager::matlist [hm_getmark mats 1]
    
      foreach i $::Mat_Manager::matlist {
         set i [hm_getcollectorname mats $i]         
         Tree $::Mat_Manager::tre add $name1/$i -image materials -popupfunc ::Mat_Manager::PopupMatMD1
      }
   }

######Directory Popup Creation#########################################################
## This procedure generates the popup menu items for the main level Database tree
#######################################################################################
proc ::Mat_Manager::PopupDirDB { tree branchesList }  {

   set popup $tree.options_popup3;
	
   if { ! [ winfo exists $popup ] } {	    
      menu $popup -tearoff 0;
      $popup add command -label "Add Material" -underline 0; #0
      $popup add separator; #1
      $popup add command -label "New Directory" -underline 0; #2
      KeepOnTop $popup;
   }		
    
   set branch [ lindex $branchesList 0 ];
   set type "base"

   $popup entryconfigure 0 -command [ list ::Mat_Manager::add_mat $type $tree $branch ];
   $popup entryconfigure 2 -command [ list ::Mat_Manager::new_dir $tree $branch ];

   set popup;
}

######Directory Popup Creation#########################################################
## This procedure generates the popup menu items for a directory
## in the Database tree
#######################################################################################
proc ::Mat_Manager::PopupDirDB1 { tree branchesList }  {

   set popup $tree.options_popup4;
	
   if { ! [ winfo exists $popup ] } {	    
      menu $popup -tearoff 0;
      $popup add command -label "Add Material" -underline 0; #0
      $popup add separator; #1
      $popup add command -label "New Directory" -underline 0; #2
      $popup add command -label "Rename Directory" -underline 0; #3
      $popup add command -label "Delete Directory" -underline 0; #4
      KeepOnTop $popup;
   }		
    
   set branch [ lindex $branchesList 0 ];
   set type "not_base"
   
   $popup entryconfigure 0 -command [ list ::Mat_Manager::add_mat $type $tree $branch ];
   $popup entryconfigure 2 -command [ list ::Mat_Manager::new_dir $tree $branch ];
   $popup entryconfigure 3 -command [ list ::Mat_Manager::rename_dir $tree $branch ];
   $popup entryconfigure 4 -command [ list ::Mat_Manager::delete_dir $tree $branch ];

   set popup;
}

   ######Add Material#####################################################################
   ## These procedures allow a material from the model to be added to the database
   #######################################################################################
   proc ::Mat_Manager::add_mat { type a b } {
    
      set new_name user1
      set winname .addMat
      
      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      
      if { $type == "base" } {
         set nam [join "$nam1" /]
      } else {
         set nam [join "/$nam1" /]
      }
      
      set ::Mat_Manager::matlist {}
      set ::Mat_Manager::matlist [hm_entitylist materials name];
         
      if { [lindex $::Mat_Manager::matlist 0] == "" } {
         tk_messageBox -icon error -message "No materials exist in the model to add to the database." -title "Add Material"
      } else {      
         if [winfo exists $winname] {
            switch -- [wm state $winname] {
               normal {
                  # Raise a buried window
                  raise $winname
               }
	       withdrawn -
	       iconic {
	          # Open and restore geometry
		  set badwindow [ catch { wm deiconify $winname } ]
		  if { $badwindow != 0 } {
		     ::Mat_Manager::main
		  }
	          catch {wm geometry $winname $::Mat_Manager::dialog(geo,$winname)}
	       }
	    }
         } else {
            CreateWindow [string trimleft $winname .] \
               windowtitle "Add Material" \
               noGeometrySaving \
               cancelButton Cancel \
               acceptButton Add \
               acceptFunc "::Mat_Manager::add_mat_md1 $winname \"$nam\"" \
               defaultButton Accept \
               propagate 1 \
               resize 0 \
               post;
         }
         
         set matlist_1 [lindex $::Mat_Manager::matlist 0];
         set ::Mat_Manager::mat_to_add $matlist_1;
       
         set recess [WindowRecess addMat];      

         set mat_add $recess.mat_add;
         hwt::AddEntry $mat_add \
            -anchor w \
            -text "$matlist_1" \
            -label "Materials:" \
            -labelwidth 10 \
            -listVar notyping fromPopDown ::Mat_Manager::matlist \
            -iconname small_arrow -iconHelp "Materials In Current Model" -iconH 0 -iconV 4 \
            -buttonWidth 12 \
            -withoutPacking \
            -entryWidth 25 \
            -state normal \
            -textvariable ::Mat_Manager::mat_to_add \
            -font [hwt::AppFont];
         
         hwt::AddPadding $recess height [ hwt::DluHeight 4 ];
         pack $mat_add -side top -fill x
      }
   }

   proc ::Mat_Manager::add_mat_md1 { winname nam args } {
      
      destroy $winname;

      set a [tk_messageBox  -icon question -type yesno -title " Add Material" -message "Do you want to add $::Mat_Manager::mat_to_add.mtl to $::Mat_Manager::matdir$nam/?"]
      if { "$a" == "yes" } {
         if { [file exists $::Mat_Manager::matdir$nam/$::Mat_Manager::mat_to_add.mtl] == 0} {
            hm_createmark mats 1 $::Mat_Manager::mat_to_add            
            *feoutput_select $::Mat_Manager::template $::Mat_Manager::matdir$nam/$::Mat_Manager::mat_to_add.mtl 1 0 0
            if { $::Mat_Manager::solver == "lsdyna" } {
               set fileid [open $::Mat_Manager::matdir/$nam/$::Mat_Manager::mat_to_add.mtl r]
               set contents [read $fileid]
               close $fileid
               set fileid [open $::Mat_Manager::matdir/$nam/$::Mat_Manager::mat_to_add.mtl w]
               puts -nonewline $fileid "$$"
               puts $fileid $contents
               close $fileid
            }
            ::Mat_Manager::Rescan
         } else {
            tk_messageBox -icon error -message "Material $::Mat_Manager::mat_to_add.mtl already exists in $::Mat_Manager::matdir$nam/." -title "Add Material"
         }
      }
   }
   
   ######New Directory####################################################################
   ## These procedures allow a directory to be created in the database
   #######################################################################################
   proc ::Mat_Manager::new_dir { a  b }  {
    
      set new_name user1
      set winname .newDir
      set ::Mat_Manager::new_dir_name ""
      
      set nam [Tree $a cget -sel]      
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]
      
      if {$nam == "/"} {
         set nam ""
      }

      if [winfo exists $winname] {
         switch -- [wm state $winname] {
            normal {
               # Raise a buried window
               raise $winname
            }
	    withdrawn -
	    iconic {
	       # Open and restore geometry
	       set badwindow [ catch { wm deiconify $winname } ]
	       if { $badwindow != 0 } {
	          ::Mat_Manager::main
	       }
	       catch {wm geometry $winname $::Mat_Manager::dialog(geo,$winname)}
	    }
         }
      } else {
         CreateWindow [string trimleft $winname .] \
            windowtitle "Create Directory" \
            noGeometrySaving \
            cancelButton Cancel \
            acceptButton Create \
            acceptFunc "::Mat_Manager::new_dir1 $winname \"$nam\"" \
            defaultButton Accept \
            propagate 1 \
            resize 0 \
            post;
      }      
       
      set recess [WindowRecess newDir];

      #set dir_label [label $recess.l1 -text "Directory Location:  $::Mat_Manager::matdir$nam"]
      
      #pack $dir_label -side top -fill x
      
      #hwt::AddPadding $recess height [ hwt::DluHeight 4 ];      

      set entry_dirname $recess.entry_dirname;
      hwt::AddEntry $entry_dirname \
         -anchor w \
         -label "Directory Name:" \
         -labelwidth 16 \
         -entrywidth 25 \
         -state normal \
         -textvariable ::Mat_Manager::new_dir_name \
         -font [hwt::AppFont];
   }

   proc ::Mat_Manager::new_dir1 { winname nam args } {
   
      destroy $winname;
      set a [tk_messageBox  -icon question -type yesno -title " Create Directory" -message "Do you want to create $::Mat_Manager::matdir$nam/$::Mat_Manager::new_dir_name?"]
      if { "$a" == "yes" } {
         if { [file isdirectory "$::Mat_Manager::matdir$nam/$::Mat_Manager::new_dir_name"] == 0} {
            catch {file mkdir "$::Mat_Manager::matdir$nam/$::Mat_Manager::new_dir_name" };
            ::Mat_Manager::Rescan
         } else {
            tk_messageBox -icon error -message "Directory $::Mat_Manager::matdir$nam/$::Mat_Manager::new_dir_name already exists." -title "Create Directory"
         }
      }
   }

   ######Rename Directo###################################################################
   ## These procedures allow a directory to be renamed in the database
   #######################################################################################
   proc ::Mat_Manager::rename_dir { a  b }  {

      set new_name user1
      set winname .renameDir
      set ::Mat_Manager::new_dir_name ""
      
      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]
      
      if [winfo exists $winname] {
         switch -- [wm state $winname] {
            normal {
               # Raise a buried window
               raise $winname
            }
	    withdrawn -
	    iconic {
	       # Open and restore geometry
	       set badwindow [ catch { wm deiconify $winname } ]
	       if { $badwindow != 0 } {
	          ::Mat_Manager::main
	       }
	       catch {wm geometry $winname $::Mat_Manager::dialog(geo,$winname)}
	    }
         }
      } else {
         CreateWindow [string trimleft $winname .] \
            windowtitle "Rename Directory" \
            noGeometrySaving \
            cancelButton Cancel \
            acceptButton Rename \
            acceptFunc "::Mat_Manager::rename_dir1 $winname \"$nam\"" \
            defaultButton Accept \
            propagate 1 \
            resize 0 \
            post;
      }      
       
      set recess [WindowRecess renameDir];
      
      #set nam1 [file dirname $nam]

      #set dir_label [label $recess.l1 -text "Current Name:    $nam"]
      
      #pack $dir_label -side top -fill x
      
      #hwt::AddPadding $recess height [ hwt::DluHeight 4 ];      

      set entry_dirname $recess.entry_dirname;
      hwt::AddEntry $entry_dirname \
         -anchor w \
         -label "New Name:" \
         -labelwidth 16 \
         -entrywidth 25 \
         -state normal \
         -textvariable ::Mat_Manager::new_dir_name \
         -font [hwt::AppFont];
   }

   proc ::Mat_Manager::rename_dir1 { winname nam args } {
   
      destroy $winname;
      set nam1 [file dirname $nam]
      if { $nam1 != "/" } {
         set nam1 "$nam1/"
      }
      set a [tk_messageBox  -icon question -type yesno -title " Rename Directory" -message "Do you want to rename $::Mat_Manager::matdir$nam to $::Mat_Manager::matdir$nam1$::Mat_Manager::new_dir_name?"]
      if { "$a" == "yes" } {
         if { [file isdirectory "$::Mat_Manager::matdir$nam1$::Mat_Manager::new_dir_name"] == 0} {
            catch {file rename $::Mat_Manager::matdir$nam  $::Mat_Manager::matdir$nam1$::Mat_Manager::new_dir_name }
            ::Mat_Manager::Rescan
         } else {
            tk_messageBox -icon error -message "Directory $::Mat_Manager::matdir$nam1$::Mat_Manager::new_dir_name already exists." -title "Rename Directory"
         }
      }
   }   
   
   ######Delete Directory#################################################################
   ## This procedure allows a directory from the database to be deleted
   ## from the database
   #######################################################################################
   proc ::Mat_Manager::delete_dir { a  b }  {

      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]

      set a [tk_messageBox  -icon question -type yesno -title " Delete Directory" -message "Do you want to delete $::Mat_Manager::matdir$nam ?"]

      if { "$a" == "yes" } {
         catch {file delete -force "$::Mat_Manager::matdir$nam"}
         ::Mat_Manager::Rescan
      }
   }

######Material Popup Creation##########################################################
## This procedure generates the popup menu items for a material in the Database tree
#######################################################################################
proc ::Mat_Manager::PopupMatDB { tree branchesList }  {

   set popup $tree.options_popup;
	
   if { ! [ winfo exists $popup ] } {
      menu $popup -tearoff 0;
      $popup add command -label "Load Material" -underline 0; #0
      #$popup add command -label "Load all solvers" -underline 0; #1
      $popup add command -label "Assign Material" -underline 0; #3
      #$popup add separator; #2
      $popup add command -label "Edit Material" -underline 0; #3
      $popup add command -label "Rename Material" -underline 0; #4      
      $popup add command -label "Delete Material" -underline 0; #5
      KeepOnTop $popup;
   }		
    
   set branch [ lindex $branchesList 0 ];
   set type "database"

    $popup entryconfigure 0 -command [ list ::Mat_Manager::load_mat_db $tree $branch ];
    #$popup entryconfigure 1 -command [ list ::Mat_Manager::update_single_mat_db $tree $branch ];
   $popup entryconfigure 1 -command [ list ::Mat_Manager::assign_single_mat_md $tree $branch $type];
   $popup entryconfigure 2 -command [ list ::Mat_Manager::edit_mat_db $tree $branch ] -state disabled;
   $popup entryconfigure 3 -command [ list ::Mat_Manager::rename_mat_db $tree $branch ];
   $popup entryconfigure 4 -command [ list ::Mat_Manager::delete_mat_db $tree $branch ];

   set popup;
}

   ######Select Material##################################################################
   ## This procedure allows a material from the database to be
   ## loaded into the current model
   #######################################################################################
   proc ::Mat_Manager::load_mat_db { a  b }  {

      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]      
      set nam1 [file tail $nam]
        ::ACF::Material::creatematifnotexist $nam1
            if { $::Mat_Manager::solver == "abaqus"} {
                *createmark materials 1 $nam1
                *deletemark materials 1
                *feinput "#abaqus/abaqus" "$::Mat_Manager::matdir$nam.mtl" 0 0 -0.01 0 0
            } elseif { $::Mat_Manager::solver == "lsdyna" || $::Mat_Manager::solver == "radioss"} {
                *dictionaryresetsolver materials "$nam1" 9
                *clearmark mats 1
                *createmark mats 1 [::ACF::Material::updatesolver $::Mat_Manager::solver [hm_getentityvalue materials $nam1 "id" 0] $::Mat_Manager::matdir$nam.mtl]
                if {[hm_marklength mats 1]>0} {
                    catch {*deletemark mats 1}
                }
            } elseif {$::Mat_Manager::solver == "pamcrash2g"} {
                *dictionaryresetsolver materials "$nam1" 18
                *clearmark mats 1
                *createmark mats 1 [::ACF::Material::updatesolver $::Mat_Manager::solver [hm_getentityvalue materials $nam1 "id" 0] $::Mat_Manager::matdir$nam.mtl]
                if {[hm_marklength mats 1]>0} {
                    catch {*deletemark mats 1}
                }
            } elseif { $::Mat_Manager::solver == "nastran"} {
                *dictionaryresetsolver materials "$nam1" 1
                *clearmark mats 1
                *createmark mats 1 [::ACF::Material::importnastranmat $nam1 $::Mat_Manager::matdir$nam.mtl]
                if {[hm_marklength mats 1]>0} {
                    catch {*deletemark mats 1}
                }
            } elseif { $::Mat_Manager::solver == "ansys"} {
                *createmark materials 1 $nam1
                *deletemark materials 1
                *feinput "#ansys/ansys" "$::Mat_Manager::matdir$nam.mtl" 0 0 -0.01 0 0
            } elseif { $::Mat_Manager::solver == "optistruct"} {
                *createmark materials 1 $nam1
                *deletemark materials 1
                *feinput "#optistruct/optistruct" "$::Mat_Manager::matdir$nam.mtl" 0 0 -0.01 0 0
            }
            tk_messageBox -icon info -message "Material $nam1 updated for $::Mat_Manager::solver." -title "Load Material"
            #::Mat_Manager::Rescan
            return 0;         
      
    }
    
    ######Update Single material###########################################################
    ## These procedures allow to update a material for all solvers
    #######################################################################################
    proc ::Mat_Manager::update_single_mat_db { a b } {
        set nam [Tree $a cget -sel]
        set nam1 [split $nam "/"]
        set nam1 [lreplace $nam1 0 1]
        set nam [join "/$nam1" /]
        set nam1 [file tail $nam]
        ::ACF::Material::creatematifnotexist $nam1
        ::ACF::Material::SingleMaterialUpdate $nam1
    }
    
    
   ######Rename Material##################################################################
   ## These procedures allow a material from the database to be renamed
   #######################################################################################
   proc ::Mat_Manager::rename_mat_db { a  b }  {
    
      set new_name user1
      set winname .renameMat
      set ::Mat_Manager::new_mat_name ""
      
      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set curname [lindex $nam1 end]      
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]      
      
      if [winfo exists $winname] {
         switch -- [wm state $winname] {
            normal {
               # Raise a buried window
               raise $winname
            }
	    withdrawn -
	    iconic {
	       # Open and restore geometry
	       set badwindow [ catch { wm deiconify $winname } ]
	       if { $badwindow != 0 } {
	          ::Mat_Manager::main
	       }
	       catch {wm geometry $winname $::Mat_Manager::dialog(geo,$winname)}
	    }
         }
      } else {
         CreateWindow [string trimleft $winname .] \
            windowtitle "Rename Material" \
            noGeometrySaving \
            cancelButton Cancel \
            acceptButton Rename \
            acceptFunc "::Mat_Manager::rename_mat_db1 $winname \"$nam\" \"$curname\"" \
            defaultButton Accept \
            propagate 1 \
            resize 0 \
            post;
      }      
       
      set recess [WindowRecess renameMat];

      #set mat_label [label $recess.l1 -text "Current Name:  $::Mat_Manager::matdir$nam.mtl"]
      
      #pack $mat_label -side top -fill x
      
      #hwt::AddPadding $recess height [ hwt::DluHeight 4 ];      

      set entry_matname $recess.entry_matname;
      hwt::AddEntry $entry_matname \
         -anchor w \
         -label "Material Name:" \
         -labelwidth 16 \
         -entrywidth 25 \
         -state normal \
         -textvariable ::Mat_Manager::new_mat_name \
         -font [hwt::AppFont];
   }

   proc ::Mat_Manager::rename_mat_db1 { winname nam curname args } {
   
      destroy $winname;
      set nam1 [file dirname $nam]
      if { $nam1 != "/" } {
         set nam1 "$nam1/"
      }
      set a [tk_messageBox  -icon question -type yesno -title " Rename Material" -message "Do you want to rename $::Mat_Manager::matdir$nam.mtl to $::Mat_Manager::matdir$nam1$::Mat_Manager::new_mat_name.mtl?"]
      if { "$a" == "yes" } {
         if { [file exists $::Mat_Manager::matdir$nam1$::Mat_Manager::new_mat_name.mtl] == 0} {
            set filenum [open $::Mat_Manager::matdir$nam.mtl r]
            set contents [read $filenum]
            close $filenum
            set rep "regsub -all {($curname)} \$contents \"$::Mat_Manager::new_mat_name\" contents"
            eval $rep
            set filenum [open $::Mat_Manager::matdir$nam.mtl w]
            puts $filenum $contents
            close $filenum
            catch { file rename -force $::Mat_Manager::matdir$nam.mtl  $::Mat_Manager::matdir$nam1$::Mat_Manager::new_mat_name.mtl }
            ::Mat_Manager::Rescan
         } else {
            tk_messageBox -icon error -message "Material $::Mat_Manager::matdir$nam1$::Mat_Manager::new_mat_name.mtl already exists in the database." -title "Rename Material"
         }
      }
   }
   
   ######Delete Material##################################################################
   ## This procedure allows a material from the database to be deleted
   #######################################################################################
   proc ::Mat_Manager::delete_mat_db { a  b }  {

      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]      

      set a [tk_messageBox  -icon question  -type yesno -title " Delete Material" -message "Do you want to delete $::Mat_Manager::matdir$nam.mtl ?"]

      if { "$a" == "yes" } {
         catch { file copy -force  "$::Mat_Manager::matdir:$nam.mtl"  "$::Mat_Manager::matdir$nam.deleted" }
         catch { file delete -force "$::Mat_Manager::matdir$nam.mtl" }
         ::Mat_Manager::Rescan
      }
   }

######Material Popup Creation##########################################################
## This procedure generates the popup menu items for the main level Model tree
#######################################################################################
proc ::Mat_Manager::PopupMatMD { tree branchesList }  {

   set popup $tree.options_popup1;
	
   if { ! [ winfo exists $popup ] } {
      menu $popup -tearoff 0;
        $popup add command -label "Create Material" -underline 0; #0
        $popup add command -label "Update All Mats" -underline 0; #0
      KeepOnTop $popup;
   }		
    
   set branch [ lindex $branchesList 0 ];

    $popup entryconfigure 0 -command [ list ::Mat_Manager::create_mat_md $tree $branch ];
    $popup entryconfigure 1 -command [ list ::Mat_Manager::update_all_mat_md $tree $branch ];

   set popup;
}

   ######Create Material##################################################################
   ## This procedure allows a material to be created in the model from scratch
   #######################################################################################
   proc ::Mat_Manager::create_mat_md { a  b }  {

      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]      
      set nam [file tail $nam]
      
      set id [hm_getitemnumber collectors "create"];      
      set owner [hm_getitemowner collectors $id];
      hm_setradiobutton collectors $owner $id;
      set id_1 [hm_getitemnumber collectors "mats"]
      set popup_id [hm_getitemowner collectors $id_1]
      hm_setpopup collectors $popup_id $id_1      
      
      hm_setinputentry collectors 12 "";
      
      hm_setpanelproc ::Mat_Manager::CreateMatPanel
   }

      proc ::Mat_Manager::CreateMatPanel { args } {
        
         wm iconify .matListWin
         
         hm_callpanel "collectors"
      
         wm deiconify .matListWin
         ::Mat_Manager::Rescan
      }
      
######Material Popup Creation##########################################################
## This procedure generates the popup menu items for a material in the Model tree
#######################################################################################
proc ::Mat_Manager::PopupMatMD1 { tree branchesList }  {

   set popup $tree.options_popup2;
	
   if { ! [ winfo exists $popup ] } {
      menu $popup -tearoff 0;
      $popup add command -label "Assign Material" -underline 0; #0
      $popup add command -label "Edit Material" -underline 0; #1
      $popup add command -label "Rename Material" -underline 0; #2
      $popup add command -label "Delete Material" -underline 0; #3
      KeepOnTop $popup;
   }		
    
   set branch [ lindex $branchesList 0 ];
   set type "model"

   $popup entryconfigure 0 -command [ list ::Mat_Manager::assign_mat_md $tree $branch $type];
   $popup entryconfigure 1 -command [ list ::Mat_Manager::edit_mat_md $tree $branch ];
   $popup entryconfigure 2 -command [ list ::Mat_Manager::rename_mat_md $tree $branch ];
   $popup entryconfigure 3 -command [ list ::Mat_Manager::delete_mat_md $tree $branch ];

   set popup;
}

   ######Assign Material##################################################################
   ## This procedure allows a material from the model to be assigned to components
   #######################################################################################
   proc ::Mat_Manager::assign_mat_md { a  b type }  {

      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]
      set nam [file tail $nam]
      
      set winname .assignMat
      set ::Mat_Manager::complist [hm_entitylist components name]
      set retval 0
      
      if { $type == "database" } {
        set retval [::Mat_Manager::load_mat_db $a $b]
      }
      
      if { $retval == 0} {
      
         if [winfo exists $winname] {
            switch -- [wm state $winname] {
               normal {
                  # Raise a buried window
                  raise $winname
               }
	       withdrawn -
	       iconic {
	          # Open and restore geometry
	          set badwindow [ catch { wm deiconify $winname } ]
	          if { $badwindow != 0 } {
	             ::Mat_Manager::main
	          }
	          catch {wm geometry $winname $::Mat_Manager::dialog(geo,$winname)}
	       }
            }
         } else {
            CreateWindow [string trimleft $winname .] \
               windowtitle "Assign Material" \
               noGeometrySaving \
               cancelButton Cancel \
               acceptButton Assign \
               acceptFunc "::Mat_Manager::accept_mat_md1 $winname \"$nam\"" \
               defaultButton Accept \
               propagate 1 \
               resize 0 \
               post;
         }
      
         set recess [WindowRecess assignMat];      

         set ::Mat_Manager::listbox_name [hwt::AddListBox $recess \
            -withBothScrolls \
            -multiselect;]
         
         $::Mat_Manager::listbox_name configure -relief raised -font [hwt::AppFont] -height 5;
         eval {$::Mat_Manager::listbox_name insert end} $::Mat_Manager::complist;
      }
   }
   
      proc ::Mat_Manager::accept_mat_md1 { winname nam args } {

         set sel [$::Mat_Manager::listbox_name curselection]
         set comps {}
         set comps1 ""
         
         destroy $winname;
         
         foreach compnum $sel {
            lappend comps [lindex $::Mat_Manager::complist $compnum]
            set comps1 "$comps1\n[lindex $::Mat_Manager::complist $compnum]"
         }

         if {[llength $comps] != 0} {
            eval *createmark comps 1 "$comps"
            *materialupdate comps 1 $nam
            
            tk_messageBox  -icon info -title " Assign Material" -message "Material $nam assigned to component(s): $comps1"
         }
      }
      
   ######Edit Material####################################################################
   ## This procedure allows a material from the model to be edited
   #######################################################################################
   proc ::Mat_Manager::edit_mat_md { a  b }  {
      
      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "/$nam1" /]      
      set ::Mat_Manager::panel_name [file tail $nam]

      hm_setpanelproc ::Mat_Manager::CreateEditPanel
   }

      proc ::Mat_Manager::CreateEditPanel { args } {
        
         wm iconify .matListWin
      
         *createmark mats 1 "$::Mat_Manager::panel_name"
      
         hm_editcard mats 1

         wm deiconify .matListWin
         ::Mat_Manager::Rescan
        }
   ###### Update All materials ###########################################################
   ## These procedures allow to update a material for all solvers
   #######################################################################################
   proc ::Mat_Manager::update_all_mat_md { a b } {
        ::ACF::Material::AllMaterialUpdate $::Mat_Manager::solver
    }
   ######Rename Material##################################################################
   ## These procedures allow a material from the model to be renamed
   #######################################################################################
   proc ::Mat_Manager::rename_mat_md { a  b }  {

      set new_name user1
      set winname .renameMat
      set ::Mat_Manager::new_mat_name ""
      
      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "$nam1" /]      
      
      if [winfo exists $winname] {
         switch -- [wm state $winname] {
            normal {
               # Raise a buried window
               raise $winname
            }
	    withdrawn -
	    iconic {
	       # Open and restore geometry
	       set badwindow [ catch { wm deiconify $winname } ]
	       if { $badwindow != 0 } {
	          ::Mat_Manager::main
	       }
	       catch {wm geometry $winname $::Mat_Manager::dialog(geo,$winname)}
	    }
         }
      } else {
         CreateWindow [string trimleft $winname .] \
            windowtitle "Rename Material" \
            noGeometrySaving \
            cancelButton Cancel \
            acceptButton Rename \
            acceptFunc "::Mat_Manager::rename_mat_md1 $winname \"$nam\"" \
            defaultButton Accept \
            propagate 1 \
            resize 0 \
            post;
      }      
       
      set recess [WindowRecess renameMat];

      #set mat_label [label $recess.l1 -text "Current Name:  $nam"]
      
      #pack $mat_label -side top -fill x
      
      #hwt::AddPadding $recess height [ hwt::DluHeight 4 ];      

      set entry_matname $recess.entry_matname;
      hwt::AddEntry $entry_matname \
         -anchor w \
         -label "Material Name:" \
         -labelwidth 16 \
         -entrywidth 25 \
         -state normal \
         -textvariable ::Mat_Manager::new_mat_name \
         -font [hwt::AppFont];
   }

   proc ::Mat_Manager::rename_mat_md1 { winname nam args } {
   
      destroy $winname;

      set a [tk_messageBox  -icon question -type yesno -title " Rename Material" -message "Do you want to rename $nam to $::Mat_Manager::new_mat_name?"]
      if { "$a" == "yes" } {
         if {[lsearch [hm_entitylist materials name] "$::Mat_Manager::new_mat_name"] == -1} {
            *renamecollector materials $nam $::Mat_Manager::new_mat_name
            ::Mat_Manager::Rescan
         } else {
            tk_messageBox -icon error -message "Material $::Mat_Manager::new_mat_name already exists in the model." -title "Rename Material"
         }
      }
   }
      
   ######Delete Material##################################################################
   ## This procedure allows a material from the model to be deleted
   #######################################################################################
   proc ::Mat_Manager::delete_mat_md { a  b }  {

      set nam [Tree $a cget -sel]
      set nam1 [split $nam "/"]
      set nam1 [lreplace $nam1 0 1]
      set nam [join "$nam1" /]      

      set a [tk_messageBox  -icon question  -type yesno -title " Delete?" -message "Do you want to delete $nam ?"]

      if { "$a" == "yes" } {
         *createmark mats 1 $nam
         *deletemark mats 1
         ::Mat_Manager::Rescan
      }
   }
   
######Set Units########################################################################
## This procedure sets the appropriate units and refreshes the materials list
#######################################################################################
proc ::Mat_Manager::SetUnits { args }  {

   set ::Mat_Manager::matdir "$::Mat_Manager::matdir_base/$::Mat_Manager::solver/$::Mat_Manager::units"
   Tree $::Mat_Manager::tre clean;
   ::Mat_Manager::create_recursive_list /
}

######Main#############################################################################
# Main program
#######################################################################################
proc ::Mat_Manager::main {} {
   
   ##Determine which template is loaded to decide which solver database to use
   set ::Mat_Manager::template [string tolower [lindex [hm_info templatefilename] 0] ];

   ##Set the default units to metric
   set ::Mat_Manager::units metric

   ##Set the root material database directory
   ##set ::Mat_Manager::matdir_base "C:/Altair/hw7.0/hm/scripts/MATERIAUX"
   set ::Mat_Manager::matdir_base "$::env(MAT_MGR)";

   if { [string first "standard" $::Mat_Manager::template] != -1 || [string first "explicit" $::Mat_Manager::template] != -1} {
      set ::Mat_Manager::solver abaqus
      set solver1 ABAQUS;
   } elseif { [string first "dyna" $::Mat_Manager::template] != -1 } {
      set ::Mat_Manager::solver lsdyna
      set solver1 LS-DYNA
   } elseif { [string first "radioss" $::Mat_Manager::template] != -1 } {
        set ::Mat_Manager::solver radioss
        set solver1 RADIOSS
   } elseif { [string first "pamcrash2g" $::Mat_Manager::template] != -1 } {
        set ::Mat_Manager::solver pamcrash2g
        set solver1 PAM2G
   } elseif { [string first "ansys" $::Mat_Manager::template] != -1 } {
      set ::Mat_Manager::solver ansys
      set solver1 ANSYS
   } elseif { [string first "general" $::Mat_Manager::template] != -1 && [string first "nastran" $::Mat_Manager::template] != -1} {
      set ::Mat_Manager::solver nastran
      set solver1 NASTRAN
   } elseif { [string first "optistruct" $::Mat_Manager::template] != -1 } {
      set ::Mat_Manager::solver optistruct
      set solver1 OPTISTRUCT
   } else {
      Message "The current template is not supported with the material database.";
      return 0; 
   }

   ##Set the material database directory
   set ::Mat_Manager::matdir "$::Mat_Manager::matdir_base/$::Mat_Manager::solver/$::Mat_Manager::units"

   ##Create the GUI   
   set ::Mat_Manager::g_MatListWin .matListWin;
   set ::Mat_Manager::tre ""
   set ::Mat_Manager::unit_list {}
   lappend ::Mat_Manager::unit_list "metric"
   lappend ::Mat_Manager::unit_list "english"
   
   if [::Mat_Manager::matlist_DialogCreate "Material Manager: $solver1" -borderwidth 1 -relief solid] {
      
      ##Make the Frames      
      pack [ frame $::Mat_Manager::g_MatListWin.line -bd 1 -height 2 -relief sunken ] \
         -after $::Mat_Manager::g_MatListWin.topFrame -side top -fill x;

        set ::Mat_Manager::recess [WindowRecess matListWin];
      
      ##Create the tree
        set ::Mat_Manager::tre [Tree $::Mat_Manager::recess create -separator /]
      ::Mat_Manager::create_recursive_list /
   
        set unit_select $::Mat_Manager::recess.unit_select;
      hwt::AddEntry $unit_select \
         -anchor w \
         -text "$::Mat_Manager::units" \
         -label "Units:" \
         -labelwidth 8 \
         -listVar notyping fromPopDown ::Mat_Manager::unit_list \
         -iconname small_arrow -iconHelp "Material Unit System" -iconH 0 -iconV 4 \
         -buttonWidth 12 \
         -withoutPacking \
         -entryWidth 8 \
         -state normal \
         -textvariable ::Mat_Manager::units \
         -selectionFunc "::Mat_Manager::SetUnits" \
         -font [hwt::AppFont];   
     
        hwt::AddPadding $::Mat_Manager::recess height [ hwt::DluHeight 4 ];
      pack $unit_select -side top -fill x
   }
}

::Mat_Manager::main
