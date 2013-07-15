function ExampleWindow( mypage ){
        LeftPosition=(screen.width)?(screen.width-950)/2:100;
        TopPosition=(screen.height)?(screen.height-450)/2:100;
        settings='width=950,height=800,top='+TopPosition+',left='+LeftPosition+',scrollbars=yes,location=no,directories=no,status=no,menubar=no,toolbar=no,resizable=no';
        var w = window.open( mypage, 'Example', settings );
        w.focus();
}
    
/**
 * checks the inner part of html for an embedded error message either from the 
 * main parameters or the uprate part, and disables the submit button if it finds one, or 
 * if the job_is_running field is set.
*/
function checkForErrors(){
        var error_count = parseInt( $( '#error_count' ).val() );
        var job_is_running = parseInt( $( '#job_is_running' ).val() );
        if(( error_count > 0 ) || ( job_is_running == 1 )){
             $( '#run_submit_button' ).attr('disabled', 'disabled');
        } else {
             $( '#run_submit_button' ).removeAttr( 'disabled' );
        }
        var up_error = $( '#uprate_error' ).val();
        if( up_error != '' ){
             $( '#uprate_error_message' ).html( "<br/><span class='input_error_message'>"+up_error+"</span>" );   
             $( '#uprate_amount' ).addClass( 'input_error' );
        } else {
             $( '#uprate_error_message' ).html( "" );   
             $( '#uprate_amount' ).removeClass( 'input_error' );
        }
}

/**
 * output pages submit; rewrites the output_target div on success
 * @param which_output: one of "quickies_page,gain_lose_page,poverty_page,inequality_page
 *
*/
function submitOutputForm( which_output ){
        var path = "/mefisto/output_page/"+which_output+"/";
        // alert( "page=|"+page );
        var these_params = $( '#outputform' ).serialize() + "&Redraw=Redraw";
        // alert( "params " + these_params );
        $.ajax( {
           type: 'post',               
           url: path,
           cache: false,
           data: these_params,
           success: function( data ) {
                   // alert( "got data as " + data );
                $( '#output_target' ).html( data );
           },
           
           error: function( jqXHR, textStatus, errorThrown){
                alert( "failed with status " + textStatus + "| error " + errorThrown );       
           }
    });
}

/**
 * run the model the quickie form
 * on success, rewrites the "quickie_div" area with a new form (for each of the 4 quickies"
 * @param which_quickie - one of '
*/
function runQuickie( which_quickie ){
        var path = "/mefisto/light_page/";
        // alert( "page=|"+page );
        var these_params = $( '#'+which_quickie+'-form' ).serialize() + '&submit_action=run';
        // alert( "params " + these_params );
        $.ajax( {
           type: 'post',               
           url: path,
           cache: false,
           data: these_params,
           success: function( data ) {
                   // alert( "got data as " + data );
                $( '#quickie_div' ).html( data );
           },
           
           error: function( jqXHR, textStatus, errorThrown){
                alert( "failed with status " + textStatus + "| error " + errorThrown );       
           }
    });
}

// fixme near dup of above
/**
 * submit the quickie form
 * on success, rewrites the "quickie_div" area with a new form (for each of the 4 quickies"
 * @param which_quickie - one of '
*/
function saveQuickie( which_quickie ){
        var path = "/mefisto/light_page/";
        // alert( "page=|"+page );
        var these_params = $( '#'+which_quickie+'-form' ).serialize() + '&submit_action=save';
        // alert( "params " + these_params );
        $.ajax( {
           type: 'post',               
           url: path,
           data: these_params,
           cache: false,
           success: function( data ) {
                   // alert( "got data as " + data );
                $( '#quickie_div' ).html( data );
           },
           
           error: function( jqXHR, textStatus, errorThrown){
                alert( "failed with status " + textStatus + "| error " + errorThrown );       
           }
    });
}

/**
 * submit the main input form via an ajax call
 * on success, refills the parameter container div
 * @param action - one of 'save', 'run', 'uprate'
*/
function submitForm( action ){
    var path = $( '#path' ).val();
    var these_params = $( '#parametersform' ).serialize();
    these_params += "&submit_action=" + action;
    $.ajax( {
           url: path,
           type: 'post',
           data: these_params,
           cache: false,
           success: function( data ) {
                $( '#parametercontainer' ).html( data );
                checkForErrors();
           },
           
           error: function( jqXHR, textStatus, errorThrown){
                alert( "failed with status " + textStatus + "| error " + errorThrown );       
           }
    });
}

function updateRow( url, table_data_id, row, action, table_container_id ){
        var target = "#"+table_container_id;
        var params = "target_params="+table_data_id+
               "&row="+row+
               "&action="+action+
               "&ajax_target_key="+table_container_id;
        $.ajax( {
               url: url,
               type: 'post',
               cache: false,
               data: params,
               success: function( data ){
                       $( target).html( data );        
               },
               error:  function( jqXHR, textStatus, errorThrown){
                       alert( "failed with status " + textStatus + "| error " + errorThrown );  
               }
           });
}

    
// 
function loadForm( path ){
    $.ajax( {
           url: path,
           cache: false,
           async: false,
           success: function( data ) {
                $( '#parametercontainer' ).html( data );       
                checkForErrors();
           },
           
           error: function( jqXHR, textStatus, errorThrown){
                alert( "failed with status " + textStatus + "| error " + errorThrown );       
           }
    });
    $( '#path' ).val( path ); // set the hidden path field 
}

// initialise the main inner parameter page on 1st load
function initialiseContainerForm(){
      if( $( "#browser" ).length > 0 ){
              $( "#browser" ).treeview();
      }
      // note we've hard-wired in the location of the 1st income tax page
      // here, as the page that's initially loaded. it might be better to
      // send back a code like 'first_page', and let the backend decide
      // what that should be. the 'length > 0 just checks for the existance of
      // the main input page Div.
      if( $("#parametercontainer").length > 0 ){
              loadForm( "/mefisto/parameters_page/income_tax/rates_and_bands/" );
      }

      //if( $('#parametersform').length > 0 ){
      //        $('#parametersform').jqTransform( {imgPath:'jqtransform/jqtransformplugin/img/' });
      //}
};

function initialiseOutputForm(){
      $( "#output_menu" ).tabs( { ajaxOptions: { cache: false }});
      
      $( "#output_submit_button" ).click(
                function( event ){
                       event.preventDefault();
                       submitOutputForm( 'run' );
                }
      );
};

function submitBlank( path ){
    $.ajax( {
           url: path,
           cache: false,
           success: function( data ) {
                window.reload();
           },
           
           error: function( jqXHR, textStatus, errorThrown){
                // alert( "failed with status " + textStatus + "| error " + errorThrown );       
           }
    });
}

// change the language by ajax calls to the language select on both the CMS and the model pages
function changeLanguageFromMyEnd( lang ){
    path = "http://www.flemosi.be/ECMS_CLIENT/pages/setlang.php?lang="+lang;
    submitBlank( path );
    // model ...
    var path = "/mefisto/lang/"+lang;
    submitBlank( path );
    // then the cms
}
