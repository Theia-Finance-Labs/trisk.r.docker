// trisk.js — UI interaction handlers for TRISK Shiny app
// Extracted from inline <script> to support strict CSP.

$(document).on("click", ".guidance-toggle-btn", function () {
  var wrapper = $(this).closest(".guidance-wrapper");
  var content = wrapper.find(".guidance-content");
  $(this).toggleClass("open");
  content.toggleClass("open");
});

// PD/EL info toggle buttons (extracted from inline onclick for CSP compliance)
$(document).on("click", ".info-toggle-btn", function () {
  var target = $(this).data("target");
  $(this).closest(".col-sm-8").find(target).slideToggle(200);
});
