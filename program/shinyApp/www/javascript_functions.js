// Function to display a message when the Shiny app is disconnected and allow the user to refresh the page
$(document).on('shiny:disconnected', function(event) {
    function checkOverlay() {
      var overlay = $('#shiny-disconnected-overlay');
      if (overlay.length) {
        console.log('Overlay found, updating content');  // Debugging line
        overlay.html(
          '<div style=\"text-align: center; line-height: 1.5;\">' +
          'Connection lost.<br>You need to <a href=\"#\" onclick=\"location.reload();\" style=\"color: #add8e6;\">refresh the page</a> to start again.<br>' +
          'There can be multiple reasons, such as an unstable internet connection. If you reproduce this behavior, ' +
          'please report the steps/clicks you took!<br>This would help all of us—developers, contributors, and users ❤️<br>' +
          'Report best through <a href=\"https://github.com/ICB-DCM/cOmicsArt/issues/new/choose\" target=\"_blank\" style=\"color: #add8e6; margin: 0 5px;\">GitHub</a> ' +
          'or email to <a href=\"mailto:cOmicsArtist@outlook.de\" style=\"color: #add8e6; margin: 0 5px;\">cOmicsArtist@outlook.de</a>.' +
          '</div>'
        );
      } else {
        setTimeout(checkOverlay, 100);  // Retry after 100ms
      }
    }
    checkOverlay();
});

// Function to get a cookie value by name
function getCookie(name) {
  const cname = name + '=';
  const decodedCookie = decodeURIComponent(document.cookie);
  const ca = decodedCookie.split(';');
  for(let i = 0; i < ca.length; i++) {
    let c = ca[i].trim();
    if (c.indexOf(cname) == 0) return c.substring(cname.length, c.length);
  }
  return '';
}

// Function to set a cookie
function setCookie(name, value, days) {
  const d = new Date();
  d.setTime(d.getTime() + (days*24*60*60*1000));
  const expires = 'expires=' + d.toUTCString();
  document.cookie = name + '=' + value + ';' + expires + ';path=/';
}

// Function to delete a cookie
function deleteCookie(name) {
  document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;';
}

// Check if the 'hasBeenBefore' cookie is present
function checkHasBeenBeforeCookie() {
  return getCookie('hasBeenBefore') === 'true';
}

// Listen for changes on the checkbox and set the cookie if checked
document.addEventListener('click', function(event) {
  if (event.target && event.target.id === 'set_cookie_checkbox') {
    const isChecked = document.getElementById('set_cookie_checkbox').checked;
    if (isChecked) {
      setCookie('hasBeenBefore', 'true', 30);
    } else {
      deleteCookie('hasBeenBefore');
    }
  }
});