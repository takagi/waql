/*
 * repl.js
 *
 */


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
  var line = dojo.create( "DIV", { class: "line" }, container, "last" );
  if ( ! line )
    error( "fail to create line DIV." );
  
  // add prompt to line
  dojo.create( "SPAN", { class: "line-prompt"
                       , innerHTML: toEntityReference( prompt ) }
                     , line, "last" );
  
  // add body to line
  dojo.create( "SPAN", { class: "line-body " + decorationClass
                       , innerHTML: toEntityReference( message ) }
                     , line, "last" );
  
  // scroll to bottom
  scrollToBottom();
}


function printInput( input, first ) {
  if ( first )
    insertLine( ">>>", input, "" );
  else
    insertLine( "...", input, "" );
  scrollToBottom();
}

function printError( message ) {
  dojo.forEach( message.split('\n'), function( line ) {
    insertLine( "   ", line, "error" );
  } );  
  scrollToBottom();
}

function printOutput( message ) {
  dojo.forEach( message.split('\n'), function( line ) {
    insertLine( "   ", line, "output" );
  } );
  scrollToBottom();
}

function showInputArea( first ) {
  // query container
  var container = dojo.query( "#container" )[0];
  if ( ! container )
    error( "container is not found." );
  
  // create input-area DIV
  var line = dojo.create( "DIV", { id: "input-area"
                                 , class: "line" }
                               , container, "last" );
  if ( ! line )
    error( "fail to create input-area DIV." );
  
  // show prompt
  if ( first )
    dojo.create( "SPAN", { class: "line-prompt"
                         , innerHTML: "&gt;&gt;&gt;" }
                       , line, "last" );
  else
    dojo.create( "SPAN", { class: "line-prompt"
                         , innerHTML: "..." }
                       , line, "last" );
  
  // show textarea
  var textarea = dojo.create( "TEXTAREA", { rows: 1 }, line, "last" );
  if ( ! textarea )
    error( "fail to create text area." );
  textarea.focus();
  textarea.onkeydown = inputAreaOnKeyDown;
  textarea.onpaste   = inputAreaOnPaste;
}

function inputAreaOnKeyDown( event ) {
  if ( event.keyCode != 13 )
    return true;
  hideInputArea();
  client.input( event.target.value );
  return false;
}

function inputAreaOnPaste( event ) {
  return false;
}
  
function hideInputArea() {
  // query input-area DIV
  var inputArea = dojo.query( "#input-area" )[0];
  if ( ! inputArea )
    error( "input-area is not found." );
  // hide input area
  dojo.destroy( inputArea );
}

function ready( first ) {
  showInputArea( first );
}


// REPL Client

var RESPONSE_CODE_BLANK    = 0;
var RESPONSE_CODE_CONTINUE = 1;
var RESPONSE_CODE_QUIT     = 2;
var RESPONSE_CODE_OUTPUT   = 3;
var RESPONSE_CODE_ERROR    = 4;

function Response( data ) {
  
  var code    = parseInt( data.substring( 0, 1 ), 10 );
  var message = "," == data.substring( 1, 2 )
              ? data.substring( 2 )
              : "";
  
  this.code = function() {
    return code;
  }
  
  this.message = function() {
    return message;
  }
}

function Client( inputCallback
               , outputCallback
               , errorCallback
               , readyCallback )
{
  
  var isFirst = true;
  
  this.input = function( input ) {
    inputCallback( input, isFirst );
    
    var xhrArgs = {
      url: "http://kamonama.mydns.jp:8080/repl?i="
           + encodeURIComponent( input ),
      handleAs: "text"
    };
    
    var deferred = dojo.xhrGet( xhrArgs );
    
    deferred.then(
      function( data ) {
        var response = new Response( data );
        switch( response.code() ) {
        case RESPONSE_CODE_BLANK:
          isFirst = true;
          readyCallback( isFirst );
          break;
        case RESPONSE_CODE_CONTINUE:
          isFirst = false;
          readyCallback( isFirst );
          break;
        case RESPONSE_CODE_QUIT:
          isFirst = true;
          readyCallback( isFirst );
          break;
        case RESPONSE_CODE_OUTPUT:
          isFirst = true;
          outputCallback( response.message() );
          readyCallback( isFirst );
          break;
        case RESPONSE_CODE_ERROR:
          isFirst = true;
          errorCallback( response.message() );
          readyCallback( isFirst );
          break;
        }
      },
      function( error ) {
        // noop
      } );
  }
  
  readyCallback( isFirst );
}


// Entry point

var client;
dojo.addOnLoad( function() {
  client = new Client( printInput, printOutput, printError, ready);
} );
