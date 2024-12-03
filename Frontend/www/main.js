
  $(document).ready(function() {
    function switchSection(section) {
      // Hide all sections
      $('.settings-section').hide();
      // Show the selected section
      $('#section_' + section).show();
      
      // Update sidebar active state
      $('.sidebar-btn').removeClass('active');
      $('.sidebar-item').removeClass('active');
      $('#nav_' + section).addClass('active');
      $('#nav_' + section + ' .sidebar-item').addClass('active');
    }
    
    // Bind click events
    $('#nav_general').on('click', function() { switchSection('general'); });
    $('#nav_model').on('click', function() { switchSection('model'); });
    $('#nav_data').on('click', function() { switchSection('data'); });
  });

//  Add this to your UI for the settings button click handler

  Shiny.addCustomMessageHandler('toggleButtons', function(message) {
    if (message.show) {
      $('#' + message.show).show();
    }
    if (message.hide) {
      $('#' + message.hide).hide();
    }
  });
  
  Shiny.addCustomMessageHandler('scrollChat', function(message) {
    const chatDiv = document.getElementById('chat_container');
    if(chatDiv) {
      chatDiv.scrollTop = chatDiv.scrollHeight;
    }
  });




$(document).ready(function() {
  // Initialize syntax highlighting
  hljs.highlightAll();
  
  // Re-run highlighting when new messages are added
  const observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.addedNodes.length) {
        hljs.highlightAll();
      }
    });
  });

  // Start observing chat container for changes
  observer.observe(document.getElementById('chat_container'), {
    childList: true,
    subtree: true
  });
});


$(document).ready(function() {
  var clipboard = new ClipboardJS('.code-copy-btn');
  var responseClipboard = new ClipboardJS('.copy-response-btn');
  
  function handleCopySuccess(e, defaultText) {
    var btn = $(e.trigger);
    var icon = btn.find('i');
    var text = btn.contents().last();
    
    btn.addClass('copy-success');
    icon.removeClass('fa-copy').addClass('fa-check');
    text.replaceWith(' Copied!');
    
    setTimeout(function() {
      btn.removeClass('copy-success');
      icon.removeClass('fa-check').addClass('fa-copy');
      text.replaceWith(defaultText);
    }, 2000);
  }
  
  clipboard.on('success', function(e) {
    handleCopySuccess(e, ' Copy code');
  });
  
  responseClipboard.on('success', function(e) {
    handleCopySuccess(e, ' Copy response');
  });
});


$(document).ready(function() {
  // Handle new chat link click
  $('.new-chat-link').on('click', function(e) {
    e.preventDefault();  // Prevent default link behavior
    Shiny.setInputValue('new_chat', Math.random());  // Trigger Shiny event
  });
});

// JavaScript for auto-expanding
$(document).ready(function() {
  const textarea = document.getElementById('user_input');
  
  function autoResize() {
    const maxHeight = 300;
    
    // Reset height to auto to get proper scrollHeight
    textarea.style.height = 'auto';
    
    // Get the required height for the content
    const scrollHeight = textarea.scrollHeight;
    
    // Set the new height, but don't exceed maxHeight
    textarea.style.height = Math.min(scrollHeight, maxHeight) + 'px';
    
    // Add scrollbar if content exceeds maxHeight
    textarea.style.overflowY = scrollHeight > maxHeight ? 'auto' : 'hidden';
  }
  
  // Bind the autoResize function to input event
  $('#user_input').on('input', autoResize);
  
  // Handle send button click
  $('#send').on('click', function() {
    // Use setTimeout to let Shiny clear the input first
    setTimeout(function() {
      textarea.style.height = 'auto';
      autoResize();
    }, 50);
  });
  
  // Initial resize
  autoResize();
});