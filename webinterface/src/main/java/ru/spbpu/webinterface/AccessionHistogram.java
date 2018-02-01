/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.spbpu.webinterface;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;

import com.vaadin.data.Binder;
import com.vaadin.event.ShortcutAction;
import com.vaadin.server.FontAwesome;
import com.vaadin.spring.annotation.SpringComponent;
import com.vaadin.spring.annotation.UIScope;
import com.vaadin.ui.Button;
import com.vaadin.ui.CssLayout;
import com.vaadin.ui.TextField;
import com.vaadin.ui.VerticalLayout;
import com.vaadin.ui.themes.ValoTheme;
import org.springframework.util.StringUtils;

import org.dussan.vaadin.dcharts.data.*;
import org.dussan.vaadin.dcharts.options.*;
import org.dussan.vaadin.dcharts.base.elements.*;
//import org.dussan.vaadin.dcharts.renderers.*;
import org.dussan.vaadin.dcharts.metadata.renderers.*;
import org.dussan.vaadin.dcharts.DCharts;

import java.util.List;

/**
 *
 * @author kkozlov
 */
//@Widgetset("com.company.OurWidgetSet")
@SpringComponent
@UIScope
public class AccessionHistogram extends VerticalLayout {
    private final AccessionRepository repository;
    
    private static final Logger log = LoggerFactory.getLogger(AccessionHistogram.class);
    
    List<Accession> accessionList;
    Binder<Accession> binder = new Binder<>(Accession.class);
    DCharts chart = new DCharts();
    
    @Autowired
    public AccessionHistogram(AccessionRepository repository) {
    	this.repository = repository;

        addComponents(chart);

	// bind using naming convention
	// binder.bindInstanceFields(this);

	// Configure and style components
	setSpacing(true);
        
        setVisible(false);
    }
    
    public final void plotHistogramAccession(String filterText) {
               
        if (StringUtils.isEmpty(filterText)) {
            accessionList = repository.findAll();
	}
	else {
            accessionList = repository.findByGenotypeStartsWithIgnoreCase(filterText);
	}
        
//	if (accessionList == null) {
//		setVisible(false);
//		return;
//	}

        log.info("Accessions found for Histogram:");
	log.info("-------------------------------");
	for (Accession accession : accessionList) {
            log.info(accession.toString());
	}
	log.info("");

        DataSeries dataSeries = new DataSeries()
            .add(1, 5, 8, 2, 3);

        SeriesDefaults seriesDefaults = new SeriesDefaults()
            .setRenderer(SeriesRenderers.BAR);

        Axes axes = new Axes()
            .addAxis(
                new XYaxis().setRenderer(AxisRenderers.CATEGORY)
                .setTicks(new Ticks()
                .add("a", "b", "c", "d", "e")));

        Highlighter highlighter = new Highlighter()
            .setShow(false);

        Options options = new Options()
            .setSeriesDefaults(seriesDefaults)
            .setAxes(axes)
            .setHighlighter(highlighter);

        chart.setDataSeries(dataSeries);
        chart.setOptions(options);
        chart.show();

	setVisible(true);

	// A hack to ensure the whole form is visible
//	chart.focus();
    }

}
