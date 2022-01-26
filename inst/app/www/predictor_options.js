$(document).ready(function() {
  $("header").find("nav").append('<span class="header-title"> <i>predicto</i>R </span>');
  document.querySelectorAll("[data-value = \'<span data-id=\"cros\"></span>\']")[0].style.display = "none";
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });

});