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
function copyBothPlotsToClipboardCombined() {
  const rawImg = $('#raw_violin_plot img')[0];
  const processedImg = $('#preprocessed_violin_plot img')[0];

  if (!rawImg || !processedImg) {
    console.error("One or both images not found.");
    Shiny.setInputValue('plot_copied_status', 'both_plots_error_nosrc_' + Date.now(), {priority: 'event'});
    return;
  }

  const canvas = document.createElement('canvas');
  const width = Math.max(rawImg.naturalWidth, processedImg.naturalWidth);
  const height = rawImg.naturalHeight + processedImg.naturalHeight;

  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext('2d');

  ctx.drawImage(rawImg, 0, 0);
  ctx.drawImage(processedImg, 0, rawImg.naturalHeight);

  canvas.toBlob(blob => {
    navigator.clipboard.write([
      new ClipboardItem({ [blob.type]: blob })
    ]).then(() => {
      console.log("Successfully copied combined plot.");
      Shiny.setInputValue('plot_copied_status', 'both_plots_success_' + Date.now(), {priority: 'event'});
    }).catch(err => {
      console.error("Failed to copy combined plot:", err);
      Shiny.setInputValue('plot_copied_status', 'both_plots_error_clipboard_' + Date.now(), {priority: 'event'});
    });
  });
}

// Add click event handlers when the document is ready
$(document).ready(function() {
  $(document).on('click', '#copy_raw_violin_plot_btn', function() {
    copyPlotToClipboard('raw_violin_plot');
  });
  
  $(document).on('click', '#copy_preprocessed_violin_plot_btn', function() {
    copyPlotToClipboard('preprocessed_violin_plot');
  });
  
  $(document).on('click', '#copy_both_plots_btn', function () {
    copyBothPlotsToClipboardCombined();
  });

  $(document).on('click', '#copy_mean_sd_plot_btn', function() {
    copyPlotToClipboard('mean_sd_plot');
  });
  
  $(document).on('click', '#sample_correlation-copy_SampleCorrelationPlot', function() {
  copyPlotToClipboard('sample_correlation-SampleCorrelationPlot');
  });
  
  $(document).on('click', '#Heatmap-copy_HeatmapPlot', function() {
    copyPlotToClipboard('Heatmap-HeatmapPlot');
  });

  $(document).on('click', '#EnrichmentAnalysis-Hallmarks-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-Hallmarks-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C1-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C1-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C2-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C2-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-CGP-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-CGP-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-CP-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-CP-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-BIOCARTA-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-BIOCARTA-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-Kegg-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-Kegg-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-PID-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-PID-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-REACTOME-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-REACTOME-EnrichmentPlot');
  });
    
  $(document).on('click', '#EnrichmentAnalysis-WIKIPATHWAYS-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-WIKIPATHWAYS-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C3-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C3-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-MIRDB-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-MIRDB-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-MIR_Legacy-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-MIR_Legacy-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-GTRD-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-GTRD-EnrichmentPlot');
  });
    
  $(document).on('click', '#EnrichmentAnalysis-TFT_Legacy-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-TFT_Legacy-EnrichmentPlot');
  });
    
  $(document).on('click', '#EnrichmentAnalysis-C4-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C4-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-CGN-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-CGN-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-CM-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-CM-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C5-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C5-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-GO-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-GO-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-GO_BP-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-GO_BP-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-GO_CC-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-GO_CC-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-GO_MF-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-GO_MF-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-HPO-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-HPO-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C6-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C6-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C7-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C7-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-IMMUNESIGDB-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-IMMUNESIGDB-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-VAX-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-VAX-EnrichmentPlot');
  });
  
  $(document).on('click', '#EnrichmentAnalysis-C8-copy_EnrichmentPlot', function() {
    copyPlotToClipboard('EnrichmentAnalysis-C8-EnrichmentPlot');
  });
});