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
// The timer is paused while the server is busy (e.g. a long analysis run), so a
// user waiting on results is not disconnected the moment they arrive.
// Note: this closes the Shiny session only; HTTP Basic Auth credentials stay
// cached by the browser until it is closed.
(function() {
  var IDLE_TIMEOUT_MS = 30 * 60 * 1000; // 30 minutes
  var idleTimer;
  var serverBusy = false;

  function resetIdleTimer() {
    clearTimeout(idleTimer);
    if (serverBusy) return;
    idleTimer = setTimeout(function() {
      if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
        Shiny.setInputValue("session_idle_timeout", Date.now());
      }
    }, IDLE_TIMEOUT_MS);
  }

  $(document).on("shiny:busy", function() {
    serverBusy = true;
    clearTimeout(idleTimer);
  });
  $(document).on("shiny:idle", function() {
    serverBusy = false;
    resetIdleTimer();
  });

  document.addEventListener("mousemove", resetIdleTimer);
  document.addEventListener("keydown", resetIdleTimer);
  document.addEventListener("click", resetIdleTimer);
  document.addEventListener("scroll", resetIdleTimer, true);
  resetIdleTimer();
})();
