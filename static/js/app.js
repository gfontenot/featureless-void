String.prototype.truncate = function (maxLength) {
  return this.length > maxLength
    ? this.substring(0, maxLength) + '…'
    : this
};

function truncateURLs() {
  $('.scream a').each(function() {
    displayTitle = this.innerText
    if (displayTitle.startsWith('http')) {
      this.innerText = displayTitle
        .replace(/https?:\/\/(w{3}.)?/g, '')
        .truncate(30)

      $(this).addClass('nowrap')
    }
  })
}

$(document).ready(truncateURLs);
