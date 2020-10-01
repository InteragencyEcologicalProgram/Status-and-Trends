// References: 
// JavaScript and jQuery: Interactive Front-End Web Development by Jon Duckett
// https://www.w3schools.com/howto/tryit.asp?filename=tryhow_css_modal_img

var modal = (function() {
	var $window = $(window);
	var $modal = $('<div id="modal" class="use-pointer"/>');
	var $content = $('<div id="modal-content" class="use-pointer"/>');
	var $bookBody = $('.book-body');
	
	var modalWidth = 500;		// choose width for modal box, in pixels
	
	$modal.append($content);
	
	$modal.on('click', function(e) {
		e.preventDefault();
		modal.close();
	});	
	
	return {
		center: function() {
			var bookBodyWidth = $bookBody.width();
			var bookBodyHeight = $bookBody.height();
			
			$content.css({
				'margin-left': Math.max((bookBodyWidth - modalWidth)/2,0), 
				'margin-right': Math.max((bookBodyWidth - modalWidth)/2,0),
				'margin-top': Math.max((bookBodyHeight - $('#modal-content').height())/2,0)
			})
		}, 		
		open: function(panel) {
			panel.content.find('img').css({
				'width': modalWidth
			});

			$content.css({
				'width': modalWidth || 'auto',
				'background-color': 'white',
				'overflow': 'auto'
			});

			$content.empty().append(panel.content);
			$bookBody.prepend($modal);

			modal.center();
			$(window).on('resize', modal.center);
		},
		close: function() {
			$content.empty();
			$modal.detach();
		}
	};
}());

(function() {
	var $panel_grids = $('.panel-grid');
	
	$panel_grids.each(function() {
		var $thisCopy = $(this).clone();
		$thisCopy.find('.expand').removeClass('expand');
		$thisCopy.find('.column800').removeClass('column800');

		// Column 1:
		var $panel_1_copy = $thisCopy.find('.col-1');
		$panel_1_copy.find('a').replaceWith($panel_1_copy.find('a').text());

		$(this).find('.col-1').find('.expand').on('click', function() {
			modal.open({content: $panel_1_copy});
		});
		
		// Column 2:
		var $panel_2_copy = $thisCopy.find('.col-2');
		$panel_2_copy.find('a').replaceWith($panel_2_copy.find('a').text());

		$(this).find('.col-2').find('.expand').on('click', function() {
			modal.open({content: $panel_2_copy});
		});
		
		// Column 3:
		var $panel_3_copy = $thisCopy.find('.col-3');
		$panel_3_copy.find('a').replaceWith($panel_3_copy.find('a').text());

		$(this).find('.col-3').find('.expand').on('click', function() {
			modal.open({content: $panel_3_copy});
		});
	});

}());

///////////////////////////////////////////////////////////////////////////////////////

// (function() {
	// var $panels = $('.panel');
	
	// $panels.each(function() {
		// // Make a copy so the original does not get removed from the page:
		// var $panelCopy = $(this).clone();
		// // Remove the expand class on the copy just in case:
		// $panelCopy.find('.expand').removeClass('expand');	
		
		// $(this).find('.expand').on('click', function() {
			// modal.open({content: $panelCopy});
		// });
	// });
// }());
