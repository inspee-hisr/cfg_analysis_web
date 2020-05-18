---
title: Map
excerpt: "This map was created using Shiny application"

---


<div class="shiny-app-frame-showcase">

    <div class="showcase-app-desc-container">
        <h1 class="showcase-app-title">COVID-19 tracker</h1>
        <div class="showcase-app-description"><p>This map contains the caves that have been sampled for their fauna as well as the protected areas of Greece.</p>

<p>This is the interactive map with the caves of Greece.</p>

</div>
        <dl class="showcase-app-meta">
            <dt><i class="fas fa-lg fa-desktop fa-fw"></i> <a href="https://savvas-paragkamian.shinyapps.io/Spatial_caves_CFG/" target="_blank">View app</a></dt>
            <dt><i class="fab fa-lg fa-github fa-fw"></i> <a href="https://github.com/inspee-hisr/CFG_analysis" target="_blank">View code</a></dt>
            
              <br>
              
            <dt><i class="fas fa-lg fa-tags fa-fw"></i> 
              
              covid19,
              
              epidemiology
              
            </dt>
        </dl>
    </div>

  <iframe src="https://savvas-paragkamian.shinyapps.io/Spatial_caves_CFG/"></iframe>
  </iframe>

</div>

<script>
  $(function() {
    // manage navbar active state
    var loc = this.location.pathname;
    loc = "/" + loc.split('/')[1];
    $('a[href="' + loc + '"]').parent().addClass('active'); 
  });
</script>



