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
        .truncate(30)
    }
  })
}

$(document).ready(truncateURLs);
