var main = function(){

/* ouverture du bloc parcours */
	$('#bouton_info').click(function(){
			
		$('#information_panel').show();
	});
	
	
	$('#bouton_fermer').click(function(){
			
		$('#information_panel').hide();		
		
	});

	$('#explorer').click(function(){
			
		$('.menu_gauche').show();
		/*$('.menu_gauche').animate(
		  {top:'0'}, 500);*/
		$('#map_col').removeClass('col-sm-12');
		$('#map_col').addClass('col-sm-9');
		$('#temps_play').removeClass('col-sm-12');
		$('#temps_play').addClass('col-sm-10');
		$('#explorer').hide();
		
	});

};

$(document).ready(main);