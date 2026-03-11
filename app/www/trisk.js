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

// Dark mode toggle with localStorage persistence
(function() {
  var STORAGE_KEY = "trisk-theme";

  function applyTheme(theme) {
    document.documentElement.setAttribute("data-theme", theme);
    var btn = document.getElementById("theme-toggle-btn");
    if (btn) {
      btn.textContent = theme === "dark" ? "\u2600 Light" : "\u263E Dark";
    }
    // Notify Shiny so Plotly charts can adapt their colors
    if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
      Shiny.setInputValue("current_theme", theme);
    }
  }

  // Apply saved theme on load (before DOM ready for flicker-free)
  var saved = localStorage.getItem(STORAGE_KEY) || "light";
  applyTheme(saved);

  // Re-apply after Shiny connects (ensures Shiny.setInputValue fires)
  $(document).on("shiny:connected", function() {
    var theme = localStorage.getItem(STORAGE_KEY) || "light";
    applyTheme(theme);
  });

  // Delegated click handler (CSP-compliant, no inline onclick)
  $(document).on("click", "#theme-toggle-btn", function() {
    var current = document.documentElement.getAttribute("data-theme") || "light";
    var next = current === "dark" ? "light" : "dark";
    localStorage.setItem(STORAGE_KEY, next);
    applyTheme(next);
  });
})();

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
