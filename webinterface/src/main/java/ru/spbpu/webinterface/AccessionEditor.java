/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.spbpu.webinterface;

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

/**
 *
 * @author kkozlov
 */
/**
 * A simple example to introduce building forms. As your real application is probably much
 * more complicated than this example, you could re-use this form in multiple places. This
 * example component is only used in VaadinUI.
 * <p>
 * In a real world application you'll most likely using a common super class for all your
 * forms - less code, better UX. See e.g. AbstractForm in Viritin
 * (https://vaadin.com/addon/viritin).
 */
@SpringComponent
@UIScope
public class AccessionEditor extends VerticalLayout {
    private final AccessionRepository repository;

    /**
     * The currently edited customer
     */
    private Accession accession;

    /* Fields to edit properties in Customer entity */
    TextField genotype = new TextField("Genotype");
    TextField env = new TextField("env");
    
    /* Action buttons */
    Button save = new Button("Save", FontAwesome.SAVE);
    Button cancel = new Button("Cancel");
    Button delete = new Button("Delete", FontAwesome.TRASH_O);
    CssLayout actions = new CssLayout(save, cancel, delete);

    Binder<Accession> binder = new Binder<>(Accession.class);

    @Autowired
    public AccessionEditor(AccessionRepository repository) {
    	this.repository = repository;

        addComponents(genotype, env, actions);

	// bind using naming convention
	binder.bindInstanceFields(this);

	// Configure and style components
	setSpacing(true);
	actions.setStyleName(ValoTheme.LAYOUT_COMPONENT_GROUP);
	save.setStyleName(ValoTheme.BUTTON_PRIMARY);
	save.setClickShortcut(ShortcutAction.KeyCode.ENTER);

        // wire action buttons to save, delete and reset
	save.addClickListener(e -> repository.save(accession));
	delete.addClickListener(e -> repository.delete(accession));
	cancel.addClickListener(e -> editAccession(accession));
	setVisible(false);
    }

    public interface ChangeHandler {

	void onChange();
    }

    public final void editAccession(Accession c) {
	if (c == null) {
		setVisible(false);
		return;
	}
	final boolean persisted = c.getNum() != null;
	if (persisted) {
		// Find fresh entity for editing
		accession = repository.findOne(c.getNum());
	}
	else {
		accession = c;
	}
	cancel.setVisible(persisted);

        // Bind customer properties to similarly named fields
	// Could also use annotation or "manual binding" or programmatically
	// moving values from fields to entities before saving
	binder.setBean(accession);

	setVisible(true);

		// A hack to ensure the whole form is visible
	save.focus();
	// Select all text in firstName field automatically
	genotype.selectAll();
    }

    public void setChangeHandler(ChangeHandler h) {
	// ChangeHandler is notified when either save or delete
	// is clicked
	save.addClickListener(e -> h.onChange());
	delete.addClickListener(e -> h.onChange());
    }
}
