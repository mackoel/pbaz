/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.spbpu.webinterface;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;

import com.vaadin.annotations.Theme;
import com.vaadin.server.FontAwesome;
import com.vaadin.server.VaadinRequest;
import com.vaadin.shared.ui.ValueChangeMode;
import com.vaadin.spring.annotation.SpringUI;
import com.vaadin.ui.Button;
import com.vaadin.ui.Grid;
import com.vaadin.ui.HorizontalLayout;
import com.vaadin.ui.TextField;
import com.vaadin.ui.UI;
import com.vaadin.ui.VerticalLayout;

/**
 *
 * @author kkozlov
 */
@SpringUI(path="/pbaz")
@Theme("valo")
public class VaadinUI extends UI {
    private final AccessionRepository repo;

    private final AccessionEditor editor;
    private final AccessionHistogram hist;

    final Grid<Accession> grid;

    final TextField filter;

    private final Button addNewBtn;
    private final Button histogramBtn;

    @Autowired
    public VaadinUI(AccessionRepository repo, AccessionEditor editor, AccessionHistogram hist) {
        this.repo = repo;
        this.editor = editor;
        this.hist = hist;
        this.grid = new Grid<>(Accession.class);
        this.filter = new TextField();
        this.addNewBtn = new Button("Accession details", FontAwesome.PLUS);
        this.histogramBtn = new Button("Show histogram", FontAwesome.PLUS);
    }

    @Override
    protected void init(VaadinRequest request) {
		// build layout
        HorizontalLayout actions = new HorizontalLayout(filter, addNewBtn, histogramBtn);
        VerticalLayout mainLayout = new VerticalLayout(actions, grid, editor);
        setContent(mainLayout);

        grid.setHeight(300, Unit.PIXELS);
        grid.setColumns("num", "genotype", "env");

        filter.setPlaceholder("Filter by genotype");

		// Hook logic to components

		// Replace listing with filtered content when user changes filter
        filter.setValueChangeMode(ValueChangeMode.LAZY);
        filter.addValueChangeListener(e -> listAccessions(e.getValue()));

		// Connect selected Customer to editor or hide if none is selected
        grid.asSingleSelect().addValueChangeListener(e -> {
                editor.editAccession(e.getValue());
            });

		// Instantiate and edit new Customer the new button is clicked
        addNewBtn.addClickListener(e -> editor.editAccession(new Accession("", "")));
        histogramBtn.addClickListener(e -> hist.plotHistogramAccession(filter.getValue()));

		// Listen changes made by the editor, refresh data from backend
        editor.setChangeHandler(() -> {
                editor.setVisible(false);
                listAccessions(filter.getValue());
            });

		// Initialize listing
        listAccessions(null);
    }

	// tag::listAccessions[]
    void listAccessions(String filterText) {
        if (StringUtils.isEmpty(filterText)) {
            grid.setItems(repo.findAll());
        }
        else {
            grid.setItems(repo.findByGenotypeStartsWithIgnoreCase(filterText));
        }
    }
	// end::listAccessions[]
}
