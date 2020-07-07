// scrape_TICKER.js
  var webPage = require('webpage');
  var page = webPage.create();
  var fs = require('fs');
  var path = 'AXP.html'
  page.open('https://financials.morningstar.com/ratios/r.html?t=AXP', function (status) {
          just_wait();
  });
  function just_wait() {
    setTimeout(function() {
              var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
    }, 2500);
  };
  
