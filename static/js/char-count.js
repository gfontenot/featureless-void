$('#scream-body').on('input', function() {
  var count = $(this).val().length
  $('#scream-length').text(count);

  if (count > 280) {
    $('#scream-length').removeClass('valid-scream').addClass('invalid-scream')
    $('#submit').prop('disabled', true)
  } else {
    $('#scream-length').removeClass('invalid-scream').addClass('valid-scream')
    $('#submit').prop('disabled', false)
  }

  if (count > 140) {
    $('#scream-length').removeClass('valid-tweet')
  } else {
    $('#scream-length').addClass('valid-tweet')
  }
});
