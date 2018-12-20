var promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true};

function promidat_model_firt(e, model, id){
  if(promidat_flat_models[model]){
    promidat_flat_models[model] = false;
    $("#"+id).click();
  }
}

 window.addEventListener("load", function(event) {
  /* Al cargarse la página  se pone el titulo */
  $("header").find("nav").append('<span class="header-title"> <i>predicto</i>R </span>');

  /* Los modelos se vuelven ejecutar al ser seleccionados una vez se haga una segmentacion diferente*/
  $("a[href^='#shiny-tab-knn']").on('click', (e) => promidat_model_firt(e,"knn","runKnn"));
  $("a[href^='#shiny-tab-dt']").on('click', (e) => promidat_model_firt(e,"dt","runDt"));
  $("a[href^='#shiny-tab-rf']").on('click', (e) => promidat_model_firt(e,"rf","runRf"));
  $("a[href^='#shiny-tab-boosting']").on('click', (e) => promidat_model_firt(e,"boosting","runBoosting"));
  $("a[href^='#shiny-tab-svm']").on('click', (e) => promidat_model_firt(e,"svm","runSvm"));
  $("#segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true};
  });
});


/// Diego ---------------------------------------------------------------------------

shinyjs.init = function() {
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
  $("[data-widget='left']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-left")
  });
  $("[data-widget='centerleft']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-centerleft")
  });
  $("[data-widget='centeright']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-centeright")
  });
  $("[data-widget='center']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-left");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-right");
    a.toggleClass("box-option-open-center")
  });
  $("[data-widget='right']").click(function() {
    var a = $(this).parents(".tab-content").first();
    a.removeClass("box-option-open-centerleft");
    a.removeClass("box-option-open-center");
    a.removeClass("box-option-open-centeright");
    a.removeClass("box-option-open-left");
    a.toggleClass("box-option-open-right")
  });

  $("ul#BoxNormal li")[2].remove();
  $("ul#BoxDisp li")[1].remove();
  $("ul#tabDyA li")[2].remove();
  $("ul#tabCor li")[2].remove();
  $("ul#BoxKnn li").last().remove();
}

Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]")
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
    }
  }
);
