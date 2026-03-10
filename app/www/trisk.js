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

// Session idle timeout — auto-close session after 30 minutes of inactivity.
// Required by bank security policies for tools handling confidential data.
(function() {
  var IDLE_TIMEOUT_MS = 30 * 60 * 1000; // 30 minutes
  var idleTimer;

  function resetIdleTimer() {
    clearTimeout(idleTimer);
    idleTimer = setTimeout(function() {
      if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
        Shiny.setInputValue("session_idle_timeout", Date.now());
      }
    }, IDLE_TIMEOUT_MS);
  }

  document.addEventListener("mousemove", resetIdleTimer);
  document.addEventListener("keypress", resetIdleTimer);
  document.addEventListener("click", resetIdleTimer);
  document.addEventListener("scroll", resetIdleTimer);
  resetIdleTimer();
})();
