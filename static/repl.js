/*
 * repl.js
 *
 */


// Managing DOM

function scrollToBottom() {

  // query container
  var container = dojo.query( "#container" )[0];
  if ( ! container )
    error( "container is not found.");
  
  // scroll to bottom
  container.scrollTop = container.scrollHeight;
}


function insertLine( prompt, message, decorationClass ) {

  // query container
  var container = dojo.query( "#container" )[0];
  if ( ! container )
    error( "container is not found." );
  
  // create new line
  var line = dojo.create( "DIV", { className: "line" }, container, "last" );
  if ( ! line )
    error( "fail to create line DIV." );
  
  // add prompt to line
  dojo.create( "CODE", { className: "line-prompt"
                       , innerHTML: toEntityReference( prompt ) }
                     , line, "last" );
  
  // add body to line
  dojo.create( "CODE", { className: "line-body " + decorationClass
                       , innerHTML: toEntityReference( message ) }
                     , line, "last" );
  
  // scroll to bottom
  scrollToBottom();
}


function printInput( input, isFirst ) {

  // show input line
  if ( isFirst )
    insertLine( ">>>", input, "" );
  else
    insertLine( "...", input, "" );

  // scroll to bottom
  scrollToBottom();
}


function printError( message ) {

  // show error messages per line
  message = message.replace( /\n$/g, "" );
  dojo.forEach( message.split('\n'), function( line ) {
    insertLine( "   ", line, "error" );
  } );

  // scroll to bottom
  scrollToBottom();
}


function printOutput( message ) {

  // show output messages per line
  message = message.replace( /\n$/g, "" );
  dojo.forEach( message.split('\n'), function( line ) {
    insertLine( "   ", line, "output" );
  } );

  // scroll to bottom
  scrollToBottom();
}


function showInputArea( isFirst, rest ) {

  // query container
  var container = dojo.query( "#container" )[0];
  if ( ! container )
    error( "container is not found." );
  
  // create input-area DIV
  var line = dojo.create( "DIV", { id: "input-area"
                                 , className: "line" }
                               , container, "last" );
  if ( ! line )
    error( "fail to create input-area DIV." );
  
  // show prompt
  if ( isFirst )
    dojo.create( "CODE", { className: "line-prompt"
                         , innerHTML: "&gt;&gt;&gt;" }
                       , line, "last" );
  else
    dojo.create( "CODE", { className: "line-prompt"
                         , innerHTML: "..." }
                       , line, "last" );
  
  // show textarea
  var textarea = dojo.create( "TEXTAREA", { rows: 1 }, line, "last" );
  if ( ! textarea )
    error( "fail to create text area." );
  textarea.focus();
  textarea.value = rest;

  // set onkeydown event to textarea
  textarea.onkeydown = function ( event ) {
    if ( event.keyCode != 13 )
      return true;
    hideInputArea();
    client.input( event.target.value, isFirst );
    return false;
  };

  // set onpaste event to textarea
  textarea.onpaste = function ( event ) {
    setTimeout( function () {
      var str = event.target.value;
      if ( -1 != str.indexOf( '\n' ) ) {
        hideInputArea();
        client.input( event.target.value, isFirst );
      }
    }, 0 );
  };

  return true;

}
  
function hideInputArea() {

  // query input-area DIV
  var inputArea = dojo.query( "#input-area" )[0];
  if ( ! inputArea )
    error( "input-area is not found." );

  // hide input area
  dojo.destroy( inputArea );
}

function ready( isFirst, rest ) {
  showInputArea( isFirst, rest );
}


// REPL Client

var RESPONSE_CODE_BLANK    = 0;
var RESPONSE_CODE_CONTINUE = 1;
var RESPONSE_CODE_QUIT     = 2;
var RESPONSE_CODE_OUTPUT   = 3;
var RESPONSE_CODE_ERROR    = 4;

function Response ( data ) {

  // unless data is string, error
  if ( typeof data != "string" )
    error( "Response data is not string." );

  // get response code
  var code    = parseInt( data.substring( 0, 1 ), 10 );

  // get response message if has
  var message = "," == data.substring( 1, 2 )
              ? data.substring( 2 )
              : "";
  
  // response code selector (read only)
  this.code = function() {
    return code;
  }

  // response message selector (read only)
  this.message = function() {
    return message;
  }
}

function Client( inputCallback
               , outputCallback
               , errorCallback
               , readyCallback )
{

  var queue = new Array();

  this.input = function ( str, isFirst ) {

    // unless str is string, error
    if ( typeof str != "string" )
      error( "Input is not string." );

    // split str into lines and enqueue them
    var lines = str.split( '\n' );
    dojo.forEach( lines, function( line ) {
      queue.push( line );
    } );

    // start processing
    this.startProcessing( isFirst );
  }

  this.startProcessing = function ( isFirst ) {

    // process one line
    this.processLine( queue.shift(), isFirst );
  }

  this.processLine = function ( line, isFirst ) {

    // input-callback
    inputCallback( line, isFirst );

    // connect to the server asynchronously
    var url = "./repl?i=" + encodeURIComponent( line );
    var xhrArgs = {
      url: url,
      handleAs: "text"
    }
    var deferred = dojo.xhrGet( xhrArgs );

    // set response handler
    var currentClient = this;
    deferred.then(
      function ( data ) {
        var response = new Response( data );
        switch ( response.code() ) {
        case RESPONSE_CODE_BLANK:
          currentClient.processed( true );
          return;
        case RESPONSE_CODE_CONTINUE:
          currentClient.processed( false );
          return;
        case RESPONSE_CODE_QUIT:
          currentClient.processed( true );
          return;
        case RESPONSE_CODE_OUTPUT:
          outputCallback( response.message() );
          currentClient.processed( true );
          return;
        case RESPONSE_CODE_ERROR:
          errorCallback( response.message() );
          currentClient.processed( true );
          return;
        default:
          error( "Invalid response code." );
          return;
        }
      },
      function ( error ) {
        errorCallback( "Could not connect to the server." );
        currentClient.processed( true );
        return;
      }
    );
  }

  this.processed = function ( isFirst ) {

    // if queue has more than one, process another
    if ( queue.length > 1 )
      this.startProcessing( isFirst );

    // if queue has only one, ready-callback passing the last line
    else if ( queue.length == 1 )
      readyCallback( isFirst, queue.shift() );

    // if queue has nothing, ready-callback
    else
      readyCallback( isFirst, "" );
  }

  // initial ready-callback
  readyCallback( true, "" );
}


// Utilities

function error( message ) {
  var error = new Error();
  error.message = message;
  throw error;
}

function toEntityReference( str ) {
  str = str.replace(/&/g,"&amp;");
  str = str.replace(/"/g,"&quot;");
  str = str.replace(/'/g,"&#039;");
  str = str.replace(/</g,"&lt;");
  str = str.replace(/>/g,"&gt;");
  str = str.replace(/ /g,"&nbsp;");
  return str;
}


// Entry point

var client;
dojo.addOnLoad( function() {
  client = new Client( printInput, printOutput, printError, ready);
} );
