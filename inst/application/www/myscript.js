var promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true};

function promidat_knn_firt(e){
  if(promidat_flat_models["knn"]){
    promidat_flat_models["knn"] = false;
    $("#runKnn").click();
  }
}

function promidat_dt_firt(e){
  if(promidat_flat_models["dt"]){
    promidat_flat_models["dt"] = false;
    $("#runDt").click();
  }
}

function promidat_rf_firt(e){
  if(promidat_flat_models["rf"]){
    promidat_flat_models["rf"] = false;
    $("#runRf").click();
  }
}

function promidat_boosting_firt(e){
  if(promidat_flat_models["boosting"]){
    promidat_flat_models["boosting"] = false;
    $("#runBoosting").click();
  }
}

function promidat_svm_firt(e){
  if(promidat_flat_models["svm"]){
    promidat_flat_models["svm"] = false;
    $("#runSvm").click();
  }
}

 window.addEventListener("load", function(event) {
  $("a[href^='#shiny-tab-knn']").on('click', promidat_knn_firt);
  $("a[href^='#shiny-tab-dt']").on('click', promidat_dt_firt);
  $("a[href^='#shiny-tab-rf']").on('click', promidat_rf_firt);
  $("a[href^='#shiny-tab-boosting']").on('click', promidat_boosting_firt);
  $("a[href^='#shiny-tab-svm']").on('click', promidat_svm_firt);
  $("#segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true};
  });
});
