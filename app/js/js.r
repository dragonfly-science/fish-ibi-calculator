
## this is not quite working re the data and field

js <- "
Shiny.addCustomMessageHandler('anim',
 function(x){

    var $icon = $('div.small-box i.fa');
    if(x == NULL && $icon.hasClass('fa-check-circle')){
      $icon.removeClass('fa-check-circle').addClass('fas fa-exclamation-triangle');
    }
    if(x != NULL && $icon.hasClass('fas fa-exclamation-triangle')){
      $icon.removeClass('fas fa-exclamation-triangle').addClass('fa-check-circle');
    }

    var $s = $('div.small-box div.inner h3');
    var o = {value: 0};
    $.Animation( o, {
        value: x
      }, {
        duration: 1500
        //easing: 'easeOutCubic'
      }).progress(function(e) {
          $s.text('$' + (e.tweens[0].now).toFixed(1));
    });

  }
);"
