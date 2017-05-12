var main = function(){

/* ouverture du bloc parcours */
	$('#bouton_info').click(function(){
			
		$('#information_panel').show();
	});
	
	
	$('#bouton_fermer').click(function(){
			
		$('#information_panel').hide();		
		
	});

};

$(document).ready(main);