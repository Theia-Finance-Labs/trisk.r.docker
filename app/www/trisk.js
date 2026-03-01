// trisk.js — UI interaction handlers for TRISK Shiny app
// Extracted from inline <script> to support strict CSP.

$(document).on("click", ".guidance-toggle-btn", function () {
  var wrapper = $(this).closest(".guidance-wrapper");
  var content = wrapper.find(".guidance-content");
  $(this).toggleClass("open");
  content.toggleClass("open");
});
