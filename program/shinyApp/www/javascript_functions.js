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

// Function to get an image blob from a URL
async function getImageBlobFromUrl(url) {
  const response = await fetch(url);
  if (!response.ok) {
    console.error('Network response was not ok for URL:', url, response.status, response.statusText);
    throw new Error('Network response was not ok for URL: ' + url);
  }
  const blob = await response.blob();
  return blob;
}

function copyPlotToClipboard(plotId) {
  console.log('Attempting to copy plot:', plotId);

  // Try <img> first (e.g. from Plotly or ggplotly)
  const imgEl = $('#' + plotId + ' img')[0];
  if (imgEl && imgEl.src) {
    return getImageBlobFromUrl(imgEl.src).then(blob => {
      return navigator.clipboard.write([
        new ClipboardItem({ [blob.type]: blob })
      ]).then(() => {
        Shiny.setInputValue('plot_copied_status', plotId + '_success_' + Date.now(), {priority: 'event'});
      }).catch(err => {
        Shiny.setInputValue('plot_copied_status', plotId + '_error_clipboard_' + Date.now(), {priority: 'event'});
        throw err;
      });
    }).catch(err => {
      Shiny.setInputValue('plot_copied_status', plotId + '_error_fetch_' + Date.now(), {priority: 'event'});
      throw err;
    });
  }

  // Fallback to canvas (for base R plots via plotOutput)
  const canvas = $('#' + plotId + ' canvas')[0];
  if (canvas) {
    return new Promise((resolve, reject) => {
      canvas.toBlob(blob => {
        if (!blob) {
          const err = new Error('Canvas toBlob failed');
          Shiny.setInputValue('plot_copied_status', plotId + '_error_toblob_' + Date.now(), {priority: 'event'});
          return reject(err);
        }

        navigator.clipboard.write([
          new ClipboardItem({ [blob.type]: blob })
        ]).then(() => {
          Shiny.setInputValue('plot_copied_status', plotId + '_success_' + Date.now(), {priority: 'event'});
          resolve();
        }).catch(err => {
          Shiny.setInputValue('plot_copied_status', plotId + '_error_clipboard_' + Date.now(), {priority: 'event'});
          reject(err);
        });
      });
    });
  }

  // Neither image nor canvas found
  Shiny.setInputValue('plot_copied_status', plotId + '_error_notfound_' + Date.now(), {priority: 'event'});
  return Promise.reject(new Error('No image or canvas found for ' + plotId));
}
async function copyBothPlotsToClipboardCombined() {
  const rawImg = $('#raw_violin_plot img')[0];
  const processedImg = $('#preprocessed_violin_plot img')[0];

  if (!rawImg || !processedImg) {
    console.error("One or both images not found.");
    Shiny.setInputValue('plot_copied_status', 'both_plots_error_nosrc_' + Date.now(), {priority: 'event'});
    return;
  }

  try {
    const canvas = document.createElement('canvas');
    const width = Math.max(rawImg.naturalWidth, processedImg.naturalWidth);
    const height = rawImg.naturalHeight + processedImg.naturalHeight;

    canvas.width = width;
    canvas.height = height;
    const ctx = canvas.getContext('2d');

    ctx.drawImage(rawImg, 0, 0);
    ctx.drawImage(processedImg, 0, rawImg.naturalHeight);

    // Convert canvas to blob using dataURL + fetch to maintain user gesture context
    // This avoids the async toBlob() which loses the user gesture
    const dataUrl = canvas.toDataURL('image/png');
    const response = await fetch(dataUrl);
    const blob = await response.blob();

    await navigator.clipboard.write([
      new ClipboardItem({ 'image/png': blob })
    ]);

    console.log("Successfully copied combined plot.");
    Shiny.setInputValue('plot_copied_status', 'both_plots_success_' + Date.now(), {priority: 'event'});
  } catch (err) {
    console.error("Failed to copy combined plot:", err);
    Shiny.setInputValue('plot_copied_status', 'both_plots_error_clipboard_' + Date.now(), {priority: 'event'});
  }
}

// Add click event handlers when the document is ready
$(document).ready(function() {
  // Generic handler for any button with data-clipboard-plot attribute
  // This replaces 50+ individual click handlers with a single generic one
  $(document).on('click', '[data-clipboard-plot]', function() {
    const plotId = $(this).data('clipboard-plot');
    copyPlotToClipboard(plotId);
  });

  // Special handler for combined plots button (uses different function)
  $(document).on('click', '#copy_both_plots_btn', function () {
    copyBothPlotsToClipboardCombined();
  });
});