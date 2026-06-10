
// get pstoken and set to input$Upload-psToken in shiny
// if the 'ICA' option is selected

function js_getPsToken(source_radio_id,source_value='ICA'){
    var checked = $('input:radio[name='+source_radio_id+']').filter('[value='+source_value+']').prop('checked');
    if(checked){
        try{
            const pstoken = document.cookie.split('; ').find(row => row.startsWith('psToken=')).split('=')[1];
            Shiny.setInputValue('Upload-psToken', pstoken, {priority: 'event'});
        }catch(error){
            return '';
        }
        // catch(error){
        //     alert('not able to get ica token!');
        // }
    }
}

// get pstoken and set to input$Upload-psToken in shiny
// if the 'ICA' option is selected

function js_getPsToken_tmp(){
    try{
        // const pstoken = "ZGVzbywwNmUwZTk0Ni0yNzI2LTM5ZjQtOWE0Ny1kN2ViNjlhOGIxZTk";
        const pstoken = document.cookie.split('; ').find(row => row.startsWith('psToken=')).split('=')[1];
        if(pstoken==""){
            return(" ");
        }
        return(pstoken);
        // Shiny.setInputValue('Upload-psToken', pstoken, {priority: 'event'});
    }catch(error){
        return ' ';
    }
}

// ask user input ica apikey 
// bind apikey to an shiny input variable
// function js_askUserIcaApikey(shiny_var)){
//     var apikey = prompt('ICA Apikey:');
//     Shiny.setInputValue(shiny_var,apikey,{priority: 'event'});
// }

$( document ).ready(function() {
    // Setting a custom handler that will
    // ask the users their name
    // then set the returned value to a Shiny input
    Shiny.addCustomMessageHandler('js_askUserIcaApikey', function(arg) {
      var apikey = prompt("ICA Apikey:");
      Shiny.setInputValue(arg.id, apikey);
    })

    // Custom handler for CE ApiKey prompt
    Shiny.addCustomMessageHandler('js_askUserCEApikey', function(arg) {
        console.log("Prompt for CE ApiKey")
        var apikey = prompt("CE Apikey:");
        Shiny.setInputValue(arg.id, apikey);
    })

    // Custom handler for CE ApiKey input setting
    Shiny.addCustomMessageHandler('js_setUserCEApikey', function(arg) {
        Shiny.setInputValue(arg.id, arg.apikey);
    })
  
    // Custom handler for updating psToken
    Shiny.addCustomMessageHandler('js_updatePsToken', function(arg) {
        const pstoken = js_getPsToken_tmp();
        console.log(pstoken)
        Shiny.setInputValue(arg.id, pstoken, {priority: 'event'});
        // Shiny.setInputValue(arg.id, js_getPsTokenTmp());
    })  
});

// Add framework to include cookies
// https://book.javascript-for-r.com/shiny-cookies.html#shiny-cookies-r-code
function getCookies(){
    var res = Cookies.get();
    if(!res.loginCount){
        Cookies.set("loginCount", Number(0));
    }
    // Auto increment login count
    Cookies.set("loginCount", Number(Number(res.loginCount)+1));

    var res = Cookies.get();
    // Check for news collapse
    collapseNews(res.loginCount);
    
    Shiny.setInputValue('cookies', res);
}

Shiny.addCustomMessageHandler('cookie-set', function(msg){
    Cookies.set(msg.name, msg.value);
    getCookies();
})
  
  Shiny.addCustomMessageHandler('cookie-remove', function(msg){
    Cookies.remove(msg.name);
    getCookies();
})

$(document).on('shiny:connected', function(ev){
    getCookies();
})

// Function to toggle news bar to collapse it
function collapseNews(loginCount){
    if(loginCount>5){
        $('#collapseExample').collapse({
            toggle: true
        })
    }
}
