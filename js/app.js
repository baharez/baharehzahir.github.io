$(document).ready(function() {
    $('.hoverZoomLink').hover(
        function() {
            $(this).css('cursor', 'pointer');
        },
        function() {
            $(this).css('cursor', 'default');
        }
    );
});